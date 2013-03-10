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

(end-tests) ;; Stop the clock and print a summary
