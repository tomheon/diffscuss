;;; diffscuss-mode.el --- Major mode for diffscuss files.

;;; Commentary:


;;; Code:

;; we need to make sure diff mode is present and loaded so we can get
;; the basic diff font-lock rules.

(require 'diff-mode)

;; If your keymap will have very few entries, then you may want to
;; consider ‘make-sparse-keymap’ rather than ‘make-keymap’.

;; (defvar diffscuss-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key map "\C-j" 'newline-and-indent)
;;     map)
;;   "Keymap for diffscuss mode")


(add-to-list 'auto-mode-alist '("\\.diffscuss\\'" . diffscuss-mode))

;; Font-lock support

;; The faces are stolen from org mode.

;;
;; TODO: Need to investigate whether I can just front-load the
;; diffscuss font lock rules and then append the diff ones like this,
;; and have that work in some of the weirder circumstances
;; (e.g. leading %'s in header material) or whether that even matters.
;; Alternatively, could duplicate all the definitions here.  But that
;; would suck.
;;

(defgroup diffscuss-faces nil
  "Faces in diffscuss-mode."
  :tag "Diffscuss Faces")

(defface diffscuss-level-1 ;; originally copied from font-lock-function-name-face
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t)))
  "Face used for diffscuss level 1 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-1 'diffscuss-level-1)

(defface diffscuss-level-2 ;; originally copied from font-lock-variable-name-face
    '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
      (((class color) (min-colors 16) (background dark))  (:foreground "LightGoldenrod"))
      (((class color) (min-colors 8)  (background light)) (:foreground "yellow"))
      (((class color) (min-colors 8)  (background dark))  (:foreground "yellow" :bold t))
      (t (:bold t)))
  "Face used for diffscuss level 2 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-2 'diffscuss-level-2)

(defface diffscuss-level-3 ;; originally copied from font-lock-keyword-face
    '((((class color) (min-colors 88) (background light)) (:foreground "Purple"))
      (((class color) (min-colors 88) (background dark))  (:foreground "Cyan1"))
      (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
      (((class color) (min-colors 16) (background dark))  (:foreground "Cyan"))
      (((class color) (min-colors 8)  (background light)) (:foreground "purple" :bold t))
      (((class color) (min-colors 8)  (background dark))  (:foreground "cyan" :bold t))
      (t (:bold t)))
  "Face used for diffscuss level 3 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-3 'diffscuss-level-3)

(defface diffscuss-level-4   ;; originally copied from font-lock-comment-face
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark))  (:foreground "chocolate1"))
      (((class color) (min-colors 16) (background light)) (:foreground "red"))
      (((class color) (min-colors 16) (background dark))  (:foreground "red1"))
      (((class color) (min-colors 8) (background light))  (:foreground "red" :bold t))
      (((class color) (min-colors 8) (background dark))   (:foreground "red" :bold t))
      (t (:bold t)))
  "Face used for difscuss level 4 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-4 'diffscuss-level-4)

(defface diffscuss-level-5 ;; originally copied from font-lock-type-face
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green")))
  "Face used for diffscuss level 5 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-5 'diffscuss-level-5)

(defface diffscuss-level-6 ;; originally copied from font-lock-constant-face
    '((((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
      (((class color) (min-colors 8)) (:foreground "magenta")))
  "Face used for diffscuss level 6 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-6 'diffscuss-level-6)

(defface diffscuss-level-7 ;; originally copied from font-lock-builtin-face
    '((((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
      (((class color) (min-colors 8)) (:foreground "blue")))
  "Face used for diffscuss level 8 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-7 'diffscuss-level-7)

(defface diffscuss-level-8 ;; originally copied from font-lock-string-face
    '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
      (((class color) (min-colors 8)) (:foreground "green")))
  "Face used for diffscuss level 8 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-8 'diffscuss-level-8)

(defvar diffscuss-font-lock-keywords
  (append
   (list
     '("^%\\*\\([ ].*\n\\|\n\\)" . diffscuss-level-1) ;; level 1 header
     '("^%-\\([ ]\\|\n\\)" . diffscuss-level-1) ;; level 1 body
     '("^%[*]\\{2\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-2) ;; level 2 header
     '("^%[-]\\{2\\}\\([ ]\\|\n\\)" . diffscuss-level-2) ;; level 2 body
     '("^%[*]\\{3\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-3) ;; level 3 header
     '("^%[-]\\{3\\}\\([ ]\\|\n\\)" . diffscuss-level-3) ;; level 3 body
     '("^%[*]\\{4\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-4) ;; level 4 header
     '("^%[-]\\{4\\}\\([ ]\\|\n\\)" . diffscuss-level-4) ;; level 4 body
     '("^%[*]\\{5\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-5) ;; level 5 header
     '("^%[-]\\{5\\}\\([ ]\\|\n\\)" . diffscuss-level-5) ;; level 5 body
     '("^%[*]\\{6\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-6) ;; level 6 header
     '("^%[-]\\{6\\}\\([ ]\\|\n\\)" . diffscuss-level-6) ;; level 6 body
     '("^%[*]\\{7\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-7) ;; level 7 header
     '("^%[-]\\{7\\}\\([ ]\\|\n\\)" . diffscuss-level-7) ;; level 7 body
     '("^%[*]\\{8\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-8) ;; level 8 header
     '("^%[-]\\{8\\}\\([ ]\\|\n\\)" . diffscuss-level-8)) ;; level 8 body
   diff-font-lock-keywords))

(defun diffscuss-mode ()
  "Major mode for inter-diff code review."
  (interactive)
  (kill-all-local-variables)
  ;; (use-local-map diffscuss-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(diffscuss-font-lock-keywords))
  (setq major-mode 'diffscuss-mode)
  (setq mode-name "Diffscuss")
  (run-hooks 'diffscuss-mode-hook))

(provide 'diffscuss-mode)

;;; diffscuss-mode.el ends here
