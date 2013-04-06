(require 'test-simple)

(test-simple-start) ;; Zero counters and start the stop watch.

;; Use (load-file) below because we want to always to read the source.
;; Also, we don't want no stinking compiled source.
(assert-t (load-file "../diffscuss-mode.el")
	  "Can't load diffscuss-mode.el - are you in the right
	  directory?" )

(note "Test parsing the leader")

(defun run-parse (line)
  "Make a temporary buffer with contents line and run
diffscuss-parse-leader against it."
  (with-temp-buffer
    (insert line)
    (beginning-of-buffer)
    (diffscuss-parse-leader)))

(defun test-parse-leader (line-and-expected-leader)
  "Run a single test of diffscuss-parse-leader"
  (let ((line (car line-and-expected-leader))
        (expected-leader (cdr line-and-expected-leader)))
    (if expected-leader
        (assert-equal expected-leader (run-parse line))
      (assert-nil (run-parse line)))))

(mapcar 'test-parse-leader
        '(("-hi\n" . nil)
          ("" . nil)
          ("%" . nil)
          (" %-- too many" . nil)
          ("%-- test" . "%--")
          ("%--" . "%--")
          ("%--- " . "%---")
          ("%- first" . "%-")
          ("%- -" . "%-")
          ("%* author: edmund" . "%*")
          ("%** something: else" . "%**")
          ("%** *something: else" . "%**")
          ))


(defun test-get-revs (text-and-expected-revs)
  ""
  (let ((text (nth 0 text-and-expected-revs))
        (old-rev (nth 1 text-and-expected-revs))
        (new-rev (nth 2 text-and-expected-revs)))
    (assert-equal old-rev (with-temp-buffer
                        (insert text)
                        (end-of-buffer)
                        (diffscuss-get-old-rev)))
    (assert-equal new-rev (with-temp-buffer
                            (insert text)
                            (end-of-buffer)
                            (diffscuss-get-new-rev)))))

(mapcar 'test-get-revs
        '(("diff --git a/diffscuss-mode/diffscuss-mode.el b/diffscuss-mode/diffscuss-mode.el\nindex eb23955..bac296b 100644\n--- a/diffscuss-mode/diffscuss-mode.el\n+++ b/diffscuss-mode/diffscuss-mode.el\n@@ -224,41 +224,6 @@\n       (end-of-line)\n       (point))))\n \n-(defun diffscuss-find-paragraph-start ()\n-  \"Return the beginning of the current comment paragraph\"\n" "eb23955" "bac296b")))

(defun test-navigation (filename initial-point diffscuss-nav-func expected-point)
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char initial-point)
    (funcall diffscuss-nav-func)
    (assert-equal expected-point (point))))

;; file with two comments in one thread
;;
;; comment 1 = 1:341
;;
;; comment 2 = 342:461
;;
;; comment by comment movement
(test-navigation "testfiles/testnav1.diffscuss" 1 'diffscuss-next-comment 342)
(test-navigation "testfiles/testnav1.diffscuss" 15 'diffscuss-next-comment 342)
(test-navigation "testfiles/testnav1.diffscuss" 342 'diffscuss-previous-comment 1)
(test-navigation "testfiles/testnav1.diffscuss" 346 'diffscuss-previous-comment 1)
(test-navigation "testfiles/testnav1.diffscuss" 342 'diffscuss-next-comment 29483)
(test-navigation "testfiles/testnav1.diffscuss" 29483 'diffscuss-previous-comment 342)
(test-navigation "testfiles/testnav1.diffscuss" 2000 'diffscuss-previous-comment 342)
;; thread by thread movements
(test-navigation "testfiles/testnav1.diffscuss" 1 'diffscuss-next-thread 29483)
(test-navigation "testfiles/testnav1.diffscuss" 15 'diffscuss-next-thread 29483)
(test-navigation "testfiles/testnav1.diffscuss" 342 'diffscuss-next-thread 29483)
(test-navigation "testfiles/testnav1.diffscuss" 346 'diffscuss-next-thread 29483)
(test-navigation "testfiles/testnav1.diffscuss" 1 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav1.diffscuss" 15 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav1.diffscuss" 342 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav1.diffscuss" 346 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav1.diffscuss" 29483 'diffscuss-previous-thread 1)

;; file with two threads of two comments each
;;
;; comment 1 = 1:341
;;
;; comment 2 = 342:461
;;
;; comment 3 = 839:955
;;
;; comment 4 = 956:1097
;;
;; comment by comment
(test-navigation "testfiles/testnav2.diffscuss" 1 'diffscuss-next-comment 342)
(test-navigation "testfiles/testnav2.diffscuss" 25 'diffscuss-next-comment 342)
(test-navigation "testfiles/testnav2.diffscuss" 342 'diffscuss-next-comment 839)
(test-navigation "testfiles/testnav2.diffscuss" 350 'diffscuss-next-comment 839)
(test-navigation "testfiles/testnav2.diffscuss" 563 'diffscuss-next-comment 839)
(test-navigation "testfiles/testnav2.diffscuss" 839 'diffscuss-next-comment 956)
(test-navigation "testfiles/testnav2.diffscuss" 850 'diffscuss-next-comment 956)
(test-navigation "testfiles/testnav2.diffscuss" 956 'diffscuss-next-comment 29742)
(test-navigation "testfiles/testnav2.diffscuss" 15000 'diffscuss-next-comment 29742)
(test-navigation "testfiles/testnav2.diffscuss" 1 'diffscuss-previous-comment 1)
(test-navigation "testfiles/testnav2.diffscuss" 25 'diffscuss-previous-comment 1)
(test-navigation "testfiles/testnav2.diffscuss" 342 'diffscuss-previous-comment 1)
(test-navigation "testfiles/testnav2.diffscuss" 350 'diffscuss-previous-comment 1)
(test-navigation "testfiles/testnav2.diffscuss" 563 'diffscuss-previous-comment 342)
(test-navigation "testfiles/testnav2.diffscuss" 839 'diffscuss-previous-comment 342)
(test-navigation "testfiles/testnav2.diffscuss" 850 'diffscuss-previous-comment 342)
(test-navigation "testfiles/testnav2.diffscuss" 956 'diffscuss-previous-comment 839)
(test-navigation "testfiles/testnav2.diffscuss" 15000 'diffscuss-previous-comment 956)
;; thread by thread
(test-navigation "testfiles/testnav2.diffscuss" 1 'diffscuss-next-thread 839)
(test-navigation "testfiles/testnav2.diffscuss" 25 'diffscuss-next-thread 839)
(test-navigation "testfiles/testnav2.diffscuss" 342 'diffscuss-next-thread 839)
(test-navigation "testfiles/testnav2.diffscuss" 350 'diffscuss-next-thread 839)
(test-navigation "testfiles/testnav2.diffscuss" 563 'diffscuss-next-thread 839)
(test-navigation "testfiles/testnav2.diffscuss" 839 'diffscuss-next-thread 29742)
(test-navigation "testfiles/testnav2.diffscuss" 850 'diffscuss-next-thread 29742)
(test-navigation "testfiles/testnav2.diffscuss" 956 'diffscuss-next-thread 29742)
(test-navigation "testfiles/testnav2.diffscuss" 15000 'diffscuss-next-thread 29742)
(test-navigation "testfiles/testnav2.diffscuss" 1 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav2.diffscuss" 25 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav2.diffscuss" 342 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav2.diffscuss" 350 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav2.diffscuss" 563 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav2.diffscuss" 839 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav2.diffscuss" 850 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav2.diffscuss" 956 'diffscuss-previous-thread 1)
(test-navigation "testfiles/testnav2.diffscuss" 15000 'diffscuss-previous-thread 839)

;; test inserting comments

(defun strip-dates (instr)
  (replace-regexp-in-string "date: .*" "DATELINE" instr))

(defun test-comment-insert (test-filename init-position comment-func expected-filename)
  (with-temp-buffer
    (insert-file-contents test-filename)
    (goto-char init-position)
    (funcall comment-func)
    (insert "NEW COMMENT TEXT")
    ;; since the testfiles get created in emacs, delete trailing
    ;; whitespace to prevent tears when only whitespace differs
    (delete-trailing-whitespace)
    (let ((actual-results (buffer-string)))
      (erase-buffer)
      (insert-file-contents expected-filename)
      (assert-equal (strip-dates (buffer-string)) (strip-dates actual-results)))))

(let ((diffscuss-author "Edmund Jorgensen")
      (diffscuss-email "tomheon@gmail.com"))
  ;; short.diffscuss has two comments in one thread
  ;;
  ;; comment one 1:113
  ;;
  ;; comment two 114:233
  ;;
  ;; File level comments
  (test-comment-insert "testfiles/short.diffscuss" 1 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-new-top-level.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 1 'diffscuss-insert-file-comment
                       "testfiles/short-with-new-top-level.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 323 'diffscuss-insert-file-comment
                       "testfiles/short-with-new-top-level.diffscuss")
  ;; replies in various forms
  (test-comment-insert "testfiles/short.diffscuss" 2 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-new-top-level-reply.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 2 'diffscuss-reply-to-comment
                       "testfiles/short-with-new-top-level-reply.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 1 'diffscuss-reply-to-comment
                       "testfiles/short-with-new-top-level-reply.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 114 'diffscuss-reply-to-comment
                       "testfiles/short-with-new-second-level-reply.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 221 'diffscuss-reply-to-comment
                       "testfiles/short-with-new-second-level-reply.diffscuss")
  ;; new comment at hunk level
  (test-comment-insert "testfiles/short.diffscuss" 234 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-new-hunk-level-comment.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 340 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-new-hunk-level-comment.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 350 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-new-hunk-level-comment.diffscuss")
  ;; new comment inter diff
  (test-comment-insert "testfiles/short.diffscuss" 362 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-new-interdiff-comment.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 364 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-new-interdiff-comment.diffscuss")
  ;; new comment at last line
  (test-comment-insert "testfiles/short.diffscuss" 372 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-last-line-comment.diffscuss")
  (test-comment-insert "testfiles/short.diffscuss" 386 'diffscuss-insert-contextual-comment
                       "testfiles/short-with-last-line-comment.diffscuss"))

(defun test-newline (test-filename init-position newline-func expected-filename)
  (with-temp-buffer
    (insert-file-contents test-filename)
    (goto-char init-position)
    (funcall newline-func)
    ;; since the testfiles get created in emacs, delete trailing
    ;; whitespace to prevent tears when only whitespace differs
    (delete-trailing-whitespace)
    (let ((actual-results (buffer-string)))
      (erase-buffer)
      (insert-file-contents expected-filename)
      (assert-equal (buffer-string) actual-results))))

;; newline stuff
(test-newline "testfiles/short.diffscuss" 1 'diffscuss-newline-and-indent
              "testfiles/short-post-open-line-top.diffscuss")
(test-newline "testfiles/short.diffscuss" 1 'diffscuss-open-line
              "testfiles/short-post-open-line-top.diffscuss")
(test-newline "testfiles/short.diffscuss" 230 'diffscuss-open-line
              "testfiles/short-post-open-body-line.diffscuss")
(test-newline "testfiles/short.diffscuss" 230 'diffscuss-newline-and-indent
              "testfiles/short-post-open-body-line.diffscuss")
(test-newline "testfiles/short.diffscuss" 222 'diffscuss-newline-and-indent
              "testfiles/short-post-open-interline.diffscuss")

(require 'cl)

;; fills
(defun test-fill (test-filename fill-positions fill-func expected-filename)
  (with-temp-buffer
    (insert-file-contents test-filename)
    (loop for pos in fill-positions
          do
          (goto-char pos)
          (funcall fill-func))
    ;; since the testfiles get created in emacs, delete trailing
    ;; whitespace to prevent tears when only whitespace differs
    (delete-trailing-whitespace)
    (let ((actual-results (buffer-string)))
      (erase-buffer)
      (insert-file-contents expected-filename)
      (assert-equal (buffer-string) actual-results))))

(test-fill "testfiles/pre-fill.diffscuss" '(518 436 432 358 268)
           'diffscuss-fill-comment-paragraph
           "testfiles/post-fill.diffscuss")

(test-fill "testfiles/pre-fill.diffscuss" '(518 436 432 358 268)
           'diffscuss-fill-paragraph
           "testfiles/post-fill.diffscuss")

(end-tests) ;; Stop the clock and print a summary
