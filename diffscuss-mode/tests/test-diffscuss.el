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


(end-tests) ;; Stop the clock and print a summary
