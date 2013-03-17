;;; diffscuss-mode.el --- Major mode for diffscuss files.

;;; Commentary:

;;; Config variables

;; By default, diffscuss will use your login name for new comments.
;; You can override that here.
(defvar diffscuss-author nil)

;;; Code:

;; we need to make sure diff mode is present and loaded so we can get
;; the basic diff font-lock rules.

(require 'diff-mode)

(defvar diffscuss-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'diffscuss-reply-to-comment)
    (define-key map "\C-c\C-i" 'diffscuss-insert-comment)
    (define-key map "\C-c\C-c" 'diffscuss-comment-or-reply)
    (define-key map "\C-c\C-s" 'diffscuss-goto-local-source)
    (define-key map "\C-c+" 'diffscuss-show-new-source)
    (define-key map "\C-c-" 'diffscuss-show-old-source)
    (define-key map "\C-j" 'diffscuss-newline-and-indent)
    (define-key map (kbd "RET") 'diffscuss-newline-and-indent)
    (define-key map "\C-o" 'diffscuss-open-line)
    map)
  "Keymap for diffscuss mode.")

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

;; Utility functions

(defun diffscuss-parse-leader ()
  "Parse the leading %[*-]+ from the current line"
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^%\\([*]+\\|[-]+\\)")
        (buffer-substring (match-beginning 0)
                          (match-end 0))
        nil)))

(defun diffscuss-comment-part-regexp (parse-leader from-string to-string)
  "Utility function to help make header / body regexps"
  (concat "^"
          (regexp-quote (replace-regexp-in-string from-string to-string parse-leader))
          "\\([ ]\\|\n\\)"))

(defun diffscuss-comment-body-regexp (parse-leader)
  "Return a regexp matching the begging of a body line leading with parse-leader"
  (diffscuss-comment-part-regexp parse-leader "*" "-"))

(defun diffscuss-comment-header-regexp (parse-leader)
  "Return a regexp matching the begging of a body line leading with parse-leader"
  (diffscuss-comment-part-regexp parse-leader "-" "*"))

(defun diffscuss-find-body-start ()
  "Return the start position of the current comment."
  (let ((comment-body-regexp
         (diffscuss-comment-body-regexp (diffscuss-parse-leader)))
        (comment-header-regexp
         (diffscuss-comment-header-regexp (diffscuss-parse-leader))))
    (save-excursion
      ;; if we're at a header, move past it
      (beginning-of-line)
      (while (and (looking-at comment-header-regexp)
                  (zerop (forward-line 1))))

      ;; move to the beginning of the first body line of the comment
      (while (and (looking-at comment-body-regexp)
                  (zerop (forward-line -1))))
      ;; in case we've moved one too far, come back.
      (or (looking-at comment-body-regexp)
          (forward-line 1))
      (point))))

(defun diffscuss-find-body-end ()
  "Return the end position of the current comment."
  (let ((comment-body-regexp
         (diffscuss-comment-body-regexp (diffscuss-parse-leader)))
        (comment-header-regexp
         (diffscuss-comment-header-regexp (diffscuss-parse-leader))))
    (save-excursion

      ;; if we're at a header, move past it
      (beginning-of-line)
      (while (and (looking-at comment-header-regexp)
                  (zerop (forward-line 1))))

      ;; move to the beginning of the last body line of the comment.
      ;; First move to the beginning of the line in case forward-line
      ;; took us to the end of the last line in the buffer.
      (beginning-of-line)
      (while (and (looking-at comment-body-regexp)
                  (zerop (forward-line 1))))

      ;; in case we've moved one too far, come back.  First move to
      ;; the beginning of the line in case forward-line took us to the
      ;; end of the last line in the buffer.
      (beginning-of-line)
      (or (looking-at comment-body-regexp)
          (forward-line -1))
      (end-of-line)
      (point))))

(defun diffscuss-find-paragraph-start ()
  "Return the beginning of the current comment paragraph"
  (let ((leader (diffscuss-parse-leader)))
    (if (diffscuss-is-body-leader leader)
        (save-excursion
          (beginning-of-line)
          ;; move to the beginning of the paragraph, that being the
          ;; first empty body line (just spaces counts as empty) or
          ;; the top of the comment body, whichever comes first.
          (let ((leader-not-para-regexp
                 (concat (regexp-quote leader) "[ ]+" "[^ \n]+")))
            (while (and (looking-at leader-not-para-regexp)
                        (zerop (forward-line -1))))
            ;; we're either on a blank line or a header line now, so
            ;; move to the next line and we're there.
            (forward-line 1)
            (point)))
      nil)))

(defun diffscuss-find-paragraph-end ()
  "Return the beginning of the current comment paragraph"
  (let ((leader (diffscuss-parse-leader)))
    (if (diffscuss-is-body-leader leader)
        (save-excursion
          (beginning-of-line)
          ;; move to the end of the paragraph, that being the first
          ;; empty body line (just spaces counts as empty) or the
          ;; bottom of the comment body, whichever comes first.
          (let ((leader-not-para-regexp
                 (concat (regexp-quote leader) "[ ]+" "[^ \n]+")))
            (while (and (looking-at leader-not-para-regexp)
                        (zerop (forward-line 1))))
            (point)))
      nil)))

(defun diffscuss-force-header (leader)
  "Return leader as a header."
  (replace-regexp-in-string "-" "*" leader))

(defun diffscuss-force-body (leader)
  "Return leader as a body."
  (replace-regexp-in-string "*" "-" leader))

(defun diffscuss-get-author ()
  "Get the author name to user for new comments."
  (if diffscuss-author
      diffscuss-author
    (user-login-name)))

(defun diffscuss-is-body-leader (leader)
  "Non-nil if leader is a body leader."
  (if leader
      (string-prefix-p "%-" leader)
    nil))

;; string-prefix-p isn't defined in Emacs 22.3.2, found this snippet online,
;; had to hack it a bit: https://github.com/magnars/s.el/issues/18

(defun backport-string-prefix-p (prefix str &optional ignore-case)
  (let ((case-fold-search ignore-case))
    (string-match (format "^%s" (regexp-quote prefix)) str)))

(unless (fboundp 'string-prefix-p)      ; not defined in Emacs 23.1
  (fset 'string-prefix-p (symbol-function 'backport-string-prefix-p)))


;; Fill logic.

(defun diffscuss-fill-comment ()
  "Fill the body of the entire current comment."
  (save-excursion
    (save-restriction
      (narrow-to-region (diffscuss-find-body-start)
                        (diffscuss-find-body-end))
      (let ((fill-prefix (concat (diffscuss-force-body (diffscuss-parse-leader))
                                 " ")))
        (fill-region (point-min) (point-max))
        ))))

(defun diffscuss-fill-comment-paragraph ()
  "Fill the current paragraph of the current comment."
  (interactive)
  (let ((leader (diffscuss-parse-leader)))
    (if (diffscuss-is-body-leader leader)
        (progn (save-excursion
                 (save-restriction
                   (narrow-to-region (diffscuss-find-paragraph-start)
                                     (diffscuss-find-paragraph-end))
                   (let ((fill-prefix (concat leader " ")))
                     (fill-region (point-min) (point-max))))))
      (message "%s" "Not in a comment paragraph."))))

(defun diffscuss-fill-paragraph (&optional justify)
  "Diffscuss sensitive replacement for fill paragraph."
  (interactive "P")
  (if (diffscuss-is-body-leader (diffscuss-parse-leader))
      (diffscuss-fill-comment-paragraph)
    ;; don't let people accidentally fill on other lines--this is a
    ;; diff after all
    t))

;; navigation

(defun diffscuss-jump-to-end-of-thread ()
  "Jump to the end of the current thread."
  (interactive)
  (if (diffscuss-parse-leader)
      (progn (beginning-of-line)
             (while (and (diffscuss-parse-leader)
                         (zerop (forward-line 1))))
             (or (diffscuss-parse-leader)
                 (forward-line -1))
             (end-of-line))
    (message "%s" "Not in a diffscuss thread")))

;; insert / reply to comment commands

(defun diffscuss-make-comment (leader)
  "Return a new comment."
  (let ((header (diffscuss-force-header leader)))
    (concat header
            "\n"
            header
            " author: "
            (diffscuss-get-author)
            "\n"
            header
            "\n"
            (diffscuss-force-body leader)
            " \n"
            (diffscuss-force-body leader)
            " ")))

(defun diffscuss-reply-to-comment ()
  "Reply to the current comment"
  (interactive)
  (let ((leader (diffscuss-parse-leader)))
    (if leader
        (progn (goto-char (diffscuss-find-body-end))
               (newline)
               (insert (diffscuss-make-comment (concat leader "*")))
               (forward-line -1)
               (end-of-line))
      (message "%s" "Not on a diffscuss comment."))))

(defun diffscuss-insert-comment ()
  "Insert a new top-level comment."
  (interactive)
  (beginning-of-line)
  (diffscuss-jump-to-end-of-thread)
  (end-of-line)
  (newline)
  (insert (diffscuss-make-comment "%*"))
  (forward-line -1)
  (end-of-line))

(defun diffscuss-insert-file-comment ()
  "Insert a file-level comment."
  (interactive)
  (beginning-of-buffer)
  (insert (diffscuss-make-comment "%*"))
  (newline)
  (forward-line -2)
  (end-of-line))

(defun diffscuss-comment-or-reply ()
  "Insert a comment or reply based on context."
  (interactive)
  ;; if at the very top of the file, insert a comment for the entire
  ;; file (meaning before any of the diff headers or lines)
  (if (= (point) 1)
      (diffscuss-insert-file-comment)
    (if (diffscuss-parse-leader)
        (diffscuss-reply-to-comment)
      (diffscuss-insert-comment))))

;; intelligent newline

(defun diffscuss-newline-and-indent ()
  "Create a new body or header line in context."
  (interactive)
  (let ((leader (diffscuss-parse-leader)))
    (if leader
        (progn (newline)
               (insert (concat leader " ")))
      (newline))))

(defun diffscuss-open-line ()
  "Open up a new body or header line in context."
  (interactive)
  (let ((leader (diffscuss-parse-leader)))
    (if leader
        (save-excursion
          (newline)
          (beginning-of-line)
          (insert (concat leader " "))))))

;; Support for jumping to source.
;;
;; Currently assumes that:
;;
;; * the diff was generated by git
;;
;; * the diffscuss file is positioned at the root of the git repo
;;
;; * the local source corresponds to the old version on the diff

(defun diffscuss-source-file-impl (old-or-new)
  (let ((line-stop-pattern nil)
        (source-name-pattern nil))
    (if (string= old-or-new "new")
        (progn (setq line-stop-pattern "^\\+\\+\\+ ")
               (setq source-name-pattern "^\\+\\+\\+ b/\\(.*\\)"))
      (progn (setq line-stop-pattern "^--- ")
             (setq source-name-pattern "^--- a/\\(.*\\)")))
    (save-excursion
      (beginning-of-line)
      (while (and (not (looking-at line-stop-pattern))
                  (zerop (forward-line -1))))
      (if (looking-at source-name-pattern)
          (buffer-substring (match-beginning 1) (match-end 1))
        nil))))

(defun diffscuss-meta-line-p ()
  "Non nil if the current line is part of hunk's meta data."
  (save-excursion
      (beginning-of-line)
      (not (looking-at "^[% +<>\n\\-]"))))

(defun diffscuss-get-source-file (old-or-new)
  "Get the name of the source file."
  (save-excursion
    (diffscuss-move-past-meta-lines)
    (diffscuss-source-file-impl old-or-new)))

(defun diffscuss-get-any-source-file (first-choice second-choice)
  (or (diffscuss-get-source-file first-choice)
      (diffscuss-get-source-file second-choice)))

(defun diffscuss-range-line-p ()
  "Non nil if line begins with @@."
  (save-excursion
    (beginning-of-line)
    (looking-at "^@@")))

(defun diffscuss-source-line-p (old-or-new)
  "Non nil if current line is part of the original source."
  (let ((line-pattern nil))
    (if (string= old-or-new "old")
        (setq line-pattern "^[ -]")
      (setq line-pattern "^[ +]"))
    (save-excursion
      (beginning-of-line)
      (looking-at line-pattern))))

(defun diffscuss-line-type-mismatch (old-or-new)
  "True if the line starts with + and old-or-new is 'old', or line starts with -
and old or new is 'new'."
  (let ((line-pattern nil))
    (if (string= old-or-new "old")
        (setq line-pattern "^\\+")
      (setq line-pattern "^-"))
  (save-excursion
    (beginning-of-line)
    (looking-at line-pattern))))


(defun diffscuss-calibrate-source-line (old-or-new)
  "Deduce what line in the source file the point is on."
  ;; TODO: this won't work if you're above the hunk line, in the
  ;; metadata stuff.
  (save-excursion
    ;; if we're looking for old, roll before +'s, if we're looking for
    ;; new, roll before -'s.
    (while (and (diffscuss-line-type-mismatch old-or-new)
                (zerop (forward-line -1))))
    (let ((source-lines 0))
      (while (and (not (diffscuss-range-line-p))
                  (zerop (forward-line -1)))
        (if (diffscuss-source-line-p old-or-new)
            (setq source-lines (+ 1 source-lines))))
      (if (diffscuss-range-line-p)
          (+ source-lines (diffscuss-parse-orig-line old-or-new))
        nil))))

(defun diffscuss-parse-orig-line (old-or-new)
  "Parse the original line out of a range header."
  (let ((line-pattern nil)
        (group-num nil))
    (if (string= old-or-new "old")
        (progn (setq line-pattern "^@@ -\\([[:digit:]]+\\)")
               (setq group-num 1))
      (progn (setq line-pattern "^@@ -[[:digit:]]+\\(,[[:digit:]]+\\)? \\+\\([[:digit:]]+\\)")
             (setq group-num 2)))
    (save-excursion
      (beginning-of-line)
      (looking-at line-pattern)
      (string-to-number (buffer-substring (match-beginning group-num) (match-end group-num))))))

(defun diffscuss-goto-local-source ()
  "Attempt to jump to the appropriate source."
  ;; TODO this needs a ton of work to tell whether the new version is
  ;; present, or the old, and which line we should land on, etc.
  ;; Right now the line guessing is terrible.
  (interactive)
  (let ((source-file (diffscuss-get-source-file "new"))
        (line-num (diffscuss-calibrate-source-line "new")))
    (if (find-file-noselect source-file)
        (if (pop-to-buffer (get-file-buffer source-file))
            (goto-line line-num))
      (progn (message "Couldn't find file %s" source-file) nil))))

(defun diffscuss-show-source-rev (source-file rev line-num old-or-new)
  "Show the old version of the source from git, and jump to the right line."
  (interactive)
  (let ((source-file-name (diffscuss-get-source-file old-or-new))
        (found-file-and-source nil))
    ;; if we can't find the source file name, it's because it's
    ;; /dev/null or some such, so don't bother looking further.
    (if source-file-name
        (progn
          (let ((source-buffer-name (concat "*" source-file-name "*" rev "*")))
            (let ((source-buffer (get-buffer source-buffer-name)))
              (if source-buffer
                  ;; we assume that if the buffer already exists, we
                  ;; had a successful run earlier.
                  (setq found-file-and-source t)
                  (progn (setq source-buffer (get-buffer-create source-buffer-name))
                         (with-current-buffer source-buffer
                           ;; this is a terrible hack to get the auto-mode-alist to work
                           ;; its magic on the buffer--it consults the file name, not the
                           ;; buffer name.
                           (setq buffer-file-name source-file-name)
                           (set-auto-mode)
                           (setq buffer-file-name nil)
                           (erase-buffer)
                           (setq got-source (process-file "git" nil source-buffer nil "show" rev))
                           (setq buffer-read-only t))
                         (if (= got-source 0)
                             (setq found-file-and-source t)
                           (kill-buffer source-buffer-name))))
              (if source-buffer
                  (progn (pop-to-buffer source-buffer)
                         (goto-line line-num)))))))
  (if (not found-file-and-source)
      (progn (message "Couldn't find source at revision %s" rev) nil))))

(defun diffscuss-show-old-source ()
  "Show the old version of the source from git, and jump to the right line."
  (interactive)
  (let ((source-file (diffscuss-get-any-source-file "old" "new"))
        (line-num (diffscuss-calibrate-source-line "old"))
        (rev (diffscuss-get-old-rev)))
    (diffscuss-show-source-rev source-file rev line-num "old")))

(defun diffscuss-show-new-source ()
  "Show the new version of the source from git, and jump to the right line."
  (interactive)
  (let ((source-file (diffscuss-get-any-source-file "new" "old"))
        (line-num (diffscuss-calibrate-source-line "new"))
        (rev (diffscuss-get-new-rev)))
    (diffscuss-show-source-rev source-file rev line-num "new")))

(defun diffscuss-move-past-meta-lines ()
  (beginning-of-line)
  (while (and (diffscuss-meta-line-p)
              (zerop (forward-line 1)))))

(defun diffscuss-get-rev (rev-regexp)
  "Get the closest revision moving up matching the rev-regexp."
  (save-excursion
    (diffscuss-move-past-meta-lines)
    ;; at this point we know we're below the index line, and can start
    ;; looking up.
    (while (and (not (looking-at "^index "))
                (zerop (forward-line -1))))
    (if (looking-at rev-regexp)
        (buffer-substring (match-beginning 1) (match-end 1))
      nil)))

(defun diffscuss-get-old-rev ()
  (diffscuss-get-rev "^index \\([^.]+\\)\\.\\."))

(defun diffscuss-get-new-rev ()
    (diffscuss-get-rev "^index [^.]+\\.\\.\\([^. \n\r]+\\)"))

;; Define the mode.

(defun diffscuss-mode ()
  "Major mode for inter-diff code review."
  (interactive)
  (kill-all-local-variables)
  (use-local-map diffscuss-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(diffscuss-font-lock-keywords t))
  (set (make-local-variable 'fill-paragraph-function) 'diffscuss-fill-paragraph)
  (setq major-mode 'diffscuss-mode)
  (setq mode-name "Diffscuss")
  (run-hooks 'diffscuss-mode-hook))

(provide 'diffscuss-mode)

;;; diffscuss-mode.el ends here
