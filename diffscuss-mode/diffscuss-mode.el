;;; diffscuss-mode.el --- Major mode for diffscuss files.

;;; Commentary:


;;; Code:

(require 'diff-mode)

;; If your keymap will have very few entries, then you may want to
;; consider ‘make-sparse-keymap’ rather than ‘make-keymap’.

;; (defvar diffscuss-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key map "\C-j" 'newline-and-indent)
;;     map)
;;   "Keymap for diffscuss mode")


(add-to-list 'auto-mode-alist '("\\.diffscuss\\'" . diffscuss-mode))

;; font-lock support
;;
;; TODO: Need to investigate whether I can just front-load the
;; diffscuss font lock rules and then append the diff ones like this,
;; and have that work in some of the weirder circumstances
;; (e.g. leading %'s in header material) or whether that even matters.
;; Alternatively, could duplicate all the definitions here.  But that
;; would suck.
;;
;; TODO: real faces, support up to--say--5 without recylcing, 10 with?

(defvar diffscuss-font-lock-keywords
  (append
   (list
     '("^%\\*\\([ ].*\n\\|\n\\)" . diff-added-face) ;; level 1 header
     '("^%-\\([ ].*\n\\|\n\\)" . diff-added-face) ;; level 1 body
     '("^%[*]\\{2\\}\\([ ].*\n\\|\n\\)" . diff-removed-face) ;; level 2 header
     '("^%[-]\\{2\\}\\([ ].*\n\\|\n\\)" . diff-removed-face)) ;; level 2 body

   diff-font-lock-keywords))

;; TODO: figure out how not to have emacs strip trailing spaces *ever*
;; in this mode?

(defun diffscuss-mode ()
  "Major mode for inter-diff code review."
  (interactive)
  (kill-all-local-variables)
  ;; (set-syntax-table wpdl-mode-syntax-table)
  ;; (use-local-map diffscuss-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(diffscuss-font-lock-keywords))
  (setq major-mode 'diffscuss-mode)
  (setq mode-name "Diffscuss")
  (run-hooks 'diffscuss-mode-hook))

(provide 'diffscuss-mode)

(provide 'diffscuss-mode)

;;; diffscuss-mode.el ends here
