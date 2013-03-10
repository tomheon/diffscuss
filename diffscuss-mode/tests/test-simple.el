;;; test-simple.el --- Simple Unit Test Framework for Emacs Lisp 
;; Rewritten from Phil Hagelberg's behave.el by rocky

;; Copyright (C) 2010, 2012 Rocky Bernstein

;; Author: Rocky Bernstein
;; URL: http://github.com/rocky/emacs-test-simple
;; Keywords: unit-test 

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:

;; test-simple.el allows you to write tests for your Emacs Lisp
;; code. Executable specifications allow you to check that your code
;; is working correctly in an automated fashion that you can use to
;; drive the focus of your development. (It's related to Test-Driven
;; Development.) You can read up on it at http://behaviour-driven.org.

;; Assertions may have docstrings so that when the specifications
;; aren't met it is easy to see what caused the failure.  

;; When "note" is used subsequent tests are grouped assumed to be
;; related to that not.

;; When you want to run the specs, evaluate the buffer. Or evaluate
;; individual assertions. Results are save in the
;; *test-simple* buffer. 

;;; Implementation

;; Contexts are stored in the *test-simple-contexts* list as structs. Each
;; context has a "specs" slot that contains a list of its specs, which
;; are stored as closures. The expect form ensures that expectations
;; are met and signals test-simple-spec-failed if they are not.

;; Warning: the variable CONTEXT is used within macros
;; in such a way that they could shadow variables of the same name in
;; the code being tested. Future versions will use gensyms to solve
;; this issue, but in the mean time avoid relying upon variables with
;; those names.

;;; To do:

;; Main issues: more expect predicates

;;; Usage:

(require 'time-date)

(eval-when-compile 
  (byte-compile-disable-warning 'cl-functions)
  ;; Somehow disabling cl-functions causes the erroneous message:
  ;;   Warning: the function `reduce' might not be defined at runtime.
  ;; FIXME: isolate, fix and/or report back to Emacs developers a bug
  ;; (byte-compile-disable-warning 'unresolved)
  (require 'cl)
  )
(require 'cl)

(defvar test-simple-debug-on-error nil
  "If non-nil raise an error on the first failure")

(defvar test-simple-verbosity 0
  "The greater the number the more verbose output")

(defstruct test-info
  description                 ;; description of last group of tests
  (assert-count 0)            ;; total number of assertions run 
  (failure-count 0)           ;; total number of failures seen
  (start-time (current-time)) ;; Time run started
  )

(defvar test-simple-info (make-test-info)
  "Variable to store testing information for a buffer")

(defun note (description &optional test-info)
  "Adds a name to a group of tests."
  (if (getenv "USE_TAP") 
    (test-simple-msg (format "# %s" description) 't)
    (if (> test-simple-verbosity 0)
	(test-simple-msg (concat "\n" description) 't))
    (unless test-info 
      (setq test-info test-simple-info))
    (setf (test-info-description test-info) description)
    ))

(defmacro test-simple-start (&optional test-start-msg)
  `(test-simple-clear nil 
		      (or ,test-start-msg
			  (if (and (functionp '__FILE__) (__FILE__))
			      (file-name-nondirectory (__FILE__))
			    (buffer-name)))
		      ))

(defun test-simple-clear (&optional test-info test-start-msg)
  "Initializes and resets everything to run tests. You should run
this before running any assertions. Running more than once clears
out information from the previous run."

  (interactive)
  
  (unless test-info 
    (unless test-simple-info 
      (make-variable-buffer-local (defvar test-simple-info (make-test-info))))
    (setq test-info test-simple-info))

  (setf (test-info-description test-info) "none set")
  (setf (test-info-start-time test-info) (current-time))
  (setf (test-info-assert-count test-info) 0)
  (setf (test-info-failure-count test-info) 0)

  (with-current-buffer (get-buffer-create "*test-simple*")
    (let ((old-read-only inhibit-read-only))
      (setq inhibit-read-only 't)
      (delete-region (point-min) (point-max))
      (if test-start-msg (insert (format "%s\n" test-start-msg)))
      (setq inhibit-read-only old-read-only)))
  (unless noninteractive
    (message "Test-Simple: test information cleared")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assertion tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assert-raises (error-condition body &optional fail-message test-info)
  (let ((fail-message (or fail-message
			  (format "assert-raises did not get expected %s" 
				  error-condition))))
    (list 'condition-case nil
	  (list 'progn body
		(list 'assert-t nil fail-message test-info "assert-raises"))
	  (list error-condition '(assert-t t)))))

(defun assert-op (op expected actual &optional fail-message test-info)
  "expectation is that ACTUAL should be equal to EXPECTED."
  (unless test-info (setq test-info test-simple-info))
  (incf (test-info-assert-count test-info))
  (if (not (funcall op actual expected))
      (let* ((fail-message 
	      (if fail-message
		  (format "Message: %s" fail-message)
		""))
	     (expect-message 
	      (format "\n  Expected: %s\n  Got: %s" expected actual))
	     (test-info-mess 
	      (if (boundp 'test-info)
		  (test-info-description test-info)
		"unset")))
	(add-failure (format "assert-%s" op) test-info-mess
		     (concat fail-message expect-message)))
    (ok-msg fail-message)))

(defun assert-equal (expected actual &optional fail-message test-info)
  "expectation is that ACTUAL should be equal to EXPECTED."
  (assert-op 'equal expected actual fail-message test-info))

(defun assert-eq (expected actual &optional fail-message test-info)
  "expectation is that ACTUAL should be EQ to EXPECTED."
  (assert-op 'eql expected actual fail-message test-info))

(defun assert-eql (expected actual &optional fail-message test-info)
  "expectation is that ACTUAL should be EQL to EXPECTED."
  (assert-op 'eql expected actual fail-message test-info))

(defun assert-matches (expected-regexp actual &optional fail-message test-info)
  "expectation is that ACTUAL should match EXPECTED-REGEXP."
  (unless test-info (setq test-info test-simple-info))
  (incf (test-info-assert-count test-info))
  (if (not (string-match expected-regexp actual))
      (let* ((fail-message 
	      (if fail-message
		  (format "\n\tMessage: %s" fail-message)
		""))
	     (expect-message 
	      (format "\tExpected Regexp: %s\n\tGot:      %s" 
		      expected-regexp actual))
	     (test-info-mess 
	      (if (boundp 'test-info)
		  (test-info-description test-info)
		"unset")))
	(add-failure "assert-equal" test-info-mess
		     (concat expect-message fail-message)))
    (progn (test-simple-msg ".") t)))

(defun assert-t (actual &optional fail-message test-info)
  "expectation is that ACTUAL is not nil."
  (assert-nil (not actual) fail-message test-info "assert-t"))

(defun assert-nil (actual &optional fail-message test-info assert-type)
  "expectation is that ACTUAL is nil. FAIL-MESSAGE is an optional
additional message to be displayed. Since several assertions
funnel down to this one, ASSERT-TYPE is an optional type."
  (unless test-info (setq test-info test-simple-info))
  (incf (test-info-assert-count test-info))
  (if actual
      (let* ((fail-message 
	      (if fail-message
		  (format "\n\tMessage: %s" fail-message)
		""))
	     (test-info-mess 
	      (if (boundp 'test-simple-info)
		  (test-info-description test-simple-info)
		"unset")))
	(add-failure "assert-nil" test-info-mess fail-message test-info))
    (ok-msg fail-message)))

(defun add-failure(type test-info-msg fail-msg &optional test-info)
  (unless test-info (setq test-info test-simple-info))
  (incf (test-info-failure-count test-info))
  (let ((failure-msg
	 (format "\nDescription: %s, type %s\n%s" test-info-msg type fail-msg))
	(old-read-only inhibit-read-only)
	)
    (save-excursion
      (not-ok-msg fail-msg)
      (test-simple-msg failure-msg 't)
      (unless noninteractive
	(if test-simple-debug-on-error
	    (signal 'test-simple-assert-failed failure-msg)
	  ;;(message failure-msg)
	  )))))

(defun end-tests (&optional test-info)
  "Give a tally of the tests run"
  (interactive)
  (unless test-info (setq test-info test-simple-info))
  (test-simple-describe-failures test-info)
  (if noninteractive 
      (progn 
	(switch-to-buffer "*test-simple*")
	(message "%s" (buffer-substring (point-min) (point-max)))
	)
    (switch-to-buffer-other-window "*test-simple*")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-simple-msg(msg &optional newline)
  (switch-to-buffer "*test-simple*")
  (let ((old-read-only inhibit-read-only))
    (setq inhibit-read-only 't)
    (insert msg)
    (if newline (insert "\n"))
    (setq inhibit-read-only old-read-only)
    (switch-to-buffer nil)
  ))

(defun ok-msg(fail-message &optional test-info)
  (unless test-info (setq test-info test-simple-info))
  (let ((msg (if (getenv "USE_TAP") 
		 (if (equal fail-message "")
		     (format "ok %d\n" (test-info-assert-count test-info))
		   (format "ok %d - %s\n" 
			   (test-info-assert-count test-info)
			   fail-message))
	       ".")))
      (test-simple-msg msg))
  't)

(defun not-ok-msg(fail-message &optional test-info)
  (unless test-info (setq test-info test-simple-info))
  (let ((msg (if (getenv "USE_TAP") 
		 (format "not ok %d\n" (test-info-assert-count test-info))
	       "F")))
      (test-simple-msg msg))
  nil)

(defun test-simple-summary-line(info)
  (let*
      ((failures (test-info-failure-count info))
       (asserts (test-info-assert-count info))
       (problems (concat (number-to-string failures) " failure" 
			 (unless (= 1 failures) "s")))
       (tests (concat (number-to-string asserts) " assertion" 
		      (unless (= 1 asserts) "s")))
       (elapsed-time (time-since (test-info-start-time info)))
       )
    (if (getenv "USE_TAP") 
	(format "1..%d" asserts)
      (format "\n%s in %s (%g seconds)" problems tests 
	      (float-time elapsed-time))
  )))

(defun test-simple-describe-failures(&optional test-info)
  (unless test-info (setq test-info test-simple-info))
  (goto-char (point-max))
  (test-simple-msg (test-simple-summary-line test-info)))

(provide 'test-simple)
;;; test-simple.el ends here
