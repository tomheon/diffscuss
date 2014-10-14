;;; diffscuss-mode.el --- Major mode for diffscuss files.

;; Copyright (c) 2014 Hut 8 Labs, LLC

;; Author: Edmund Jorgensen <edmund@hut8labs.com>
;; Keywords: tools

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Code:

;;; Config variables

(defvar diffscuss-author nil
  "Author name for new comments.
By default, diffscuss will use your git config user.name for new
comments.  You can override that here.")

(defvar diffscuss-email nil
  "Email address for new comments.
By default, diffscuss will use your git config user.email for new
comments.  You can override that here.")

(defvar diffscuss-git-exe (executable-find "git")
  "Path to git executable.")

(defvar diffscuss-exe "diffscuss"
  "The top-level diffscuss command to run.
If you haven't installed diffscuss somewhere in your path, you
can override this to provide the full path to it.")

(defvar diffscuss-prefix-key "\C-c\C-d"
  "The key prefix used for diffscuss. This prefixes the entire
keyboard map for use in diffscuss-mode.")

;;; Code:

;; we need to make sure diff mode is present and loaded so we can get
;; the basic diff font-lock rules.

(require 'diff-mode)

(defvar diffscuss-mode-map
  (let* ((map (make-sparse-keymap)))
    (defun define-diffscuss-key (keys func)
      (define-key map (concat diffscuss-prefix-key keys) func))
    
    ;; insert comments
    (define-diffscuss-key "\C-r" 'diffscuss-reply-to-comment)
    (define-diffscuss-key "\C-i" 'diffscuss-insert-comment)
    (define-diffscuss-key "\C-f" 'diffscuss-insert-file-comment)
    (define-diffscuss-key "\C-c" 'diffscuss-insert-contextual-comment)
    
    ;; showing source
    (define-diffscuss-key "s" 'diffscuss-goto-local-source)
    (define-diffscuss-key "+" 'diffscuss-show-new-source)
    (define-diffscuss-key "-" 'diffscuss-show-old-source)
    
    ;; newline stuff
    (define-key map "\C-j" 'diffscuss-newline-and-indent)
    (define-key map (kbd "RET") 'diffscuss-newline-and-indent)
    (define-key map "\C-o" 'diffscuss-open-line)
    
    ;; navigation
    (define-diffscuss-key "f" 'diffscuss-next-comment)
    (define-diffscuss-key "b" 'diffscuss-previous-comment)
    (define-diffscuss-key "n" 'diffscuss-next-thread)
    (define-diffscuss-key "p" 'diffscuss-previous-thread)
    (define-diffscuss-key "a" 'diffscuss-jump-to-beginning-of-thread)
    (define-diffscuss-key "e" 'diffscuss-jump-to-end-of-thread)
    
    ;; diffscuss mailbox integration
    (define-diffscuss-key "mp" 'diffscuss-mb-post)
    (define-diffscuss-key "mb" 'diffscuss-mb-bounce)
    (define-diffscuss-key "md" 'diffscuss-mb-done)

    map)
  "Keymap for diffscuss mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.diffscuss\\'" . diffscuss-mode))

;; Font-lock support

;; The faces are stolen from org mode.

;;
;; TODO: Need to investigate whether I can just front-load the
;; diffscuss font lock rules and then append the diff ones like this,
;; and have that work in some of the weirder circumstances
;; (e.g. leading #'s in header material) or whether that even matters.
;; Alternatively, could duplicate all the definitions here.  But that
;; would suck.
;;

(defgroup diffscuss-faces nil
  "Faces in diffscuss-mode."
  :tag "Diffscuss Faces"
  :group 'diffscuss)

(defface diffscuss-level-1 ;; originally copied from font-lock-function-name-face
    '((t (:inherit outline-1)))
  "Face used for diffscuss level 1 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-1 'diffscuss-level-1)

(defface diffscuss-level-2 ;; originally copied from font-lock-variable-name-face
    '((t (:inherit outline-2)))
  "Face used for diffscuss level 2 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-2 'diffscuss-level-2)

(defface diffscuss-level-3 ;; originally copied from font-lock-keyword-face
    '((t (:inherit outline-3)))
  "Face used for diffscuss level 3 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-3 'diffscuss-level-3)

(defface diffscuss-level-4   ;; originally copied from font-lock-comment-face
    '((t (:inherit outline-4)))
  "Face used for difscuss level 4 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-4 'diffscuss-level-4)

(defface diffscuss-level-5 ;; originally copied from font-lock-type-face
    '((t (:inherit outline-5)))
  "Face used for diffscuss level 5 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-5 'diffscuss-level-5)

(defface diffscuss-level-6 ;; originally copied from font-lock-constant-face
    '((t (:inherit outline-6)))
  "Face used for diffscuss level 6 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-6 'diffscuss-level-6)

(defface diffscuss-level-7 ;; originally copied from font-lock-builtin-face
    '((t (:inherit outline-7)))
  "Face used for diffscuss level 8 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-7 'diffscuss-level-7)

(defface diffscuss-level-8 ;; originally copied from font-lock-string-face
    '((t (:inherit outline-8)))
  "Face used for diffscuss level 8 comments."
  :group 'diffscuss-faces)

(defvar diffscuss-level-8 'diffscuss-level-8)

(defvar diffscuss-font-lock-keywords
  (append
   (list
     '("^#\\*\\([ ].*\n\\|\n\\)" . diffscuss-level-1) ;; level 1 header
     '("^#-\\([ ]\\|\n\\|$\\)" . diffscuss-level-1) ;; level 1 body
     '("^#[*]\\{2\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-2) ;; level 2 header
     '("^#[-]\\{2\\}\\([ ]\\|\n\\|$\\)" . diffscuss-level-2) ;; level 2 body
     '("^#[*]\\{3\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-3) ;; level 3 header
     '("^#[-]\\{3\\}\\([ ]\\|\n\\|$\\)" . diffscuss-level-3) ;; level 3 body
     '("^#[*]\\{4\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-4) ;; level 4 header
     '("^#[-]\\{4\\}\\([ ]\\|\n\\|$\\)" . diffscuss-level-4) ;; level 4 body
     '("^#[*]\\{5\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-5) ;; level 5 header
     '("^#[-]\\{5\\}\\([ ]\\|\n\\|$\\)" . diffscuss-level-5) ;; level 5 body
     '("^#[*]\\{6\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-6) ;; level 6 header
     '("^#[-]\\{6\\}\\([ ]\\|\n\\|$\\)" . diffscuss-level-6) ;; level 6 body
     '("^#[*]\\{7\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-7) ;; level 7 header
     '("^#[-]\\{7\\}\\([ ]\\|\n\\|$\\)" . diffscuss-level-7) ;; level 7 body
     '("^#[*]\\{8\\}\\([ ].*\n\\|\n\\)" . diffscuss-level-8) ;; level 8 header
     '("^#[-]\\{8\\}\\([ ]\\|\n\\|$\\)" . diffscuss-level-8)) ;; level 8 body
   diff-font-lock-keywords))

;; Utility functions

(defun diffscuss-parse-leader ()
  "Parse the leading #[*-]+ from the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^#\\([*]+\\|[-]+\\)")
        (buffer-substring (match-beginning 0)
                          (match-end 0))
        nil)))

(defun diffscuss-comment-part-regexp (parse-leader from-string to-string)
  "Utility function to help make header / body regexps."
  (concat "^"
          (regexp-quote (replace-regexp-in-string from-string to-string parse-leader))
          "\\([ ]\\|\n\\|$\\)"))

(defun diffscuss-comment-body-regexp (parse-leader)
  "Return a regexp matching the begging of a body line leading with parse-leader."
  (diffscuss-comment-part-regexp parse-leader "*" "-"))

(defun diffscuss-comment-header-regexp (parse-leader)
  "Return a regexp matching the begging of a body line leading with parse-leader."
  (diffscuss-comment-part-regexp parse-leader "-" "*"))

(defun diffscuss-find-body-start ()
  "Return the start position of the current comment's body."
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

(defun diffscuss-find-comment-start ()
  "Return the start position of the current comment."
  (if (not (diffscuss-parse-leader))
      nil
    (save-excursion
      (goto-char (diffscuss-find-body-start))
      (let ((comment-header-regexp
             (diffscuss-comment-header-regexp (diffscuss-parse-leader))))
        (beginning-of-line)
        (forward-line -1)
        (while (and (looking-at comment-header-regexp)
                    (zerop (forward-line -1))))
        (if (not (looking-at comment-header-regexp))
            (forward-line 1))
        (point)))))

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
  "Return the beginning of the current comment paragraph."
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
  "Return the beginning of the current comment paragraph."
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
    (diffscuss-git-user-name)))

(defun diffscuss-get-email ()
  "Get the author email to user for new comments."
  (if diffscuss-email
      diffscuss-email
    (diffscuss-git-user-email)))

(defun diffscuss-git-user-name ()
  "Get the git user name."
  (diffscuss-get-git-config "user.name"))

(defun diffscuss-git-user-email ()
  "Get the git user email."
  (diffscuss-get-git-config "user.email"))

(defun diffscuss-get-git-config (config-name)
  (with-temp-buffer
    (call-process diffscuss-git-exe nil t nil "config" "--get" config-name)
    (diffscuss-trim-string (buffer-string))))

(defun diffscuss-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
(replace-regexp-in-string "\\`[ \t\n]*" ""
                          (replace-regexp-in-string "[ \t\n]*\\'" "" string)))


(defun diffscuss-is-body-leader (leader)
  "Non-nil if leader is a body leader."
  (if leader
      (string-prefix-p "#-" leader)
    nil))

;; string-prefix-p isn't defined in Emacs 22.3.2, found this snippet online,
;; had to hack it a bit: https://github.com/magnars/s.el/issues/18

(defun backport-string-prefix-p (prefix str &optional ignore-case)
  (let ((case-fold-search ignore-case))
    (string-match (format "^%s" (regexp-quote prefix)) str)))

(unless (fboundp 'string-prefix-p)      ; not defined in Emacs 23.1
  (fset 'string-prefix-p (symbol-function 'backport-string-prefix-p)))

;; Fill logic.

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

;; insert / reply to comment commands

(defun diffscuss-get-date-time ()
  "Get the current local date and time in ISO 8601."
  (format-time-string "%Y-%m-%dT%T%z"))

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
            " email: "
            (diffscuss-get-email)
            "\n"
            header
            " date: "
            (diffscuss-get-date-time)
            "\n"
            header
            "\n"
            (diffscuss-force-body leader)
            " \n"
            (diffscuss-force-body leader)
            " ")))

(defun diffscuss-jump-to-reply-spot ()
  "Jump to where a reply should naturally fit."
  (let ((leader (diffscuss-parse-leader)))
    (if leader
        (progn (goto-char (diffscuss-find-body-end))
               (beginning-of-line)
               (forward-line 1)
               (while (and
                       (diffscuss-parse-leader)
                       (> (length (diffscuss-parse-leader))
                           (length leader))
                       (zerop (forward-line 1))))
               ;; calibrate for end of file or not
               (forward-line -1)
               (goto-char (diffscuss-find-body-end))))))

(defun diffscuss-reply-to-comment ()
  "Reply to the current comment"
  (interactive)
  (let ((leader (diffscuss-parse-leader)))
    (if leader
        (progn (diffscuss-jump-to-reply-spot)
               (end-of-line)
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
  (insert (diffscuss-make-comment "#*"))
  (forward-line -1)
  (end-of-line))

(defun diffscuss-insert-file-comment ()
  "Insert a file-level comment."
  (interactive)
  (goto-char (point-min))
  ;; either there's already a file level comment, in which case we
  ;; want to jump in at the bottom of the thread, or not.
  (let ((existing-comment (diffscuss-parse-leader)))
    (if existing-comment
        (progn (diffscuss-jump-to-end-of-thread)
               (newline)))
    (insert (diffscuss-make-comment "#*"))
    (if existing-comment
        ;; we're already positioned at the end of the comment, we only
        ;; need to move back 1.
        (forward-line -1)
        (progn (newline)
               (forward-line -2)))
    (end-of-line)))

(defun diffscuss-in-header-p ()
  "True if we're in the header material."
  ;; if we travel up until we hit a meta line, we'll hit a range line
  ;; first if we're not in a header, otherwise we'll hit a different
  ;; meta line.
  (save-excursion
    (while (and (not (diffscuss-meta-line-p))
                (zerop (forward-line -1))))
    (not (diffscuss-range-line-p))))

(defun diffscuss-insert-contextual-comment ()
  "Insert a comment or reply based on context."
  (interactive)
  ;; if at the very top of the file, insert a comment for the entire
  ;; file (meaning before any of the diff headers or lines)
  (if (= (point) 1)
      (diffscuss-insert-file-comment)
    ;; otherwise, if we're already in a comment, reply to it.
    (if (diffscuss-parse-leader)
        (diffscuss-reply-to-comment)
      ;; if we're on a meta piece, go just past it
      (if (diffscuss-in-header-p)
          (progn (while (and (not (diffscuss-range-line-p))
                             (zerop (forward-line 1))))
                 (diffscuss-insert-comment))
        ;; otherwise, new top-level comment.
        (diffscuss-insert-comment)))))

;; intelligent newline

(defun diffscuss-newline-and-indent ()
  "Create a new body or header line in context."
  (interactive)
  (let ((leader (diffscuss-parse-leader)))
    (if leader
        (progn
          (if (< (current-column) (length leader))
              (move-to-column (length leader)))
          (newline)
          (insert leader)
          (if (not (looking-at " "))
              (insert " ")))
      (newline))))

(defun diffscuss-open-line ()
  "Open up a new body or header line in context."
  (interactive)
  (let ((leader (diffscuss-parse-leader)))
    (if leader
        (save-excursion
          (diffscuss-newline-and-indent))
      (open-line 1))))

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
      (not (looking-at "^[# +\n\\-]"))))

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
  (interactive)
  (let ((outbuf-name (generate-new-buffer-name "diffscuss-local-source")))
    (if (/= 0 (call-process-region (point-min)
                                   (point-max)
                                   diffscuss-exe
                                   nil
                                   outbuf-name
                                   nil
                                   "find-local"
                                   (number-to-string (line-number-at-pos))))
        (with-current-buffer outbuf-name
          (message "%s" (buffer-string))
          (message "Could not find local source"))
      (progn
        (let ((to-find-fname nil)
              (to-find-line nil))
          (with-current-buffer outbuf-name
            (setq to-find-fname (mapconcat
                                 'identity
                                 (butlast (split-string (buffer-string) " ") 1) " "))
            (setq to-find-line (car (last (split-string (buffer-string) " ") 1))))
          (pop-to-buffer (find-file-noselect to-find-fname))
          (goto-line (string-to-number to-find-line))))
      (with-current-buffer outbuf-name
        (kill-buffer outbuf-name)))))


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
            (let ((source-buffer (get-buffer source-buffer-name))
                  got-source)
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
                           (setq got-source (process-file diffscuss-git-exe nil source-buffer nil "show" rev))
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
             (end-of-line))))

(defun diffscuss-jump-to-beginning-of-thread ()
  "Jump to the beginning of the current thread."
  (interactive)
  (if (diffscuss-parse-leader)
      (progn (beginning-of-line)
             (while (and (diffscuss-parse-leader)
                         (zerop (forward-line -1))))
             (or (diffscuss-parse-leader)
                 (forward-line 1)))))

(defun diffscuss-next-thread ()
  "Jump to the beginning of the next thread."
  (interactive)
  (diffscuss-jump-to-end-of-thread)
  (diffscuss-next-comment)
  (recenter))

(defun diffscuss-previous-thread ()
  "Jump to the beginning of the previous thread."
  (interactive)
  (diffscuss-jump-to-beginning-of-thread)
  (diffscuss-previous-comment)
  (diffscuss-jump-to-beginning-of-thread)
  (recenter))

(defun diffscuss-next-comment ()
  "Jump to the next comment."
  (interactive)
  (if (diffscuss-parse-leader)
      (goto-char (diffscuss-find-body-end)))
  (beginning-of-line)
  (forward-line 1)
  (while (and (not (diffscuss-parse-leader))
              (zerop (forward-line 1))))
  (recenter))

(defun diffscuss-string-ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun diffscuss-previous-comment ()
  "Jump to the previous comment."
  (interactive)
  (if (diffscuss-parse-leader)
      (goto-char (diffscuss-find-comment-start)))
  (beginning-of-line)
  (forward-line -1)
  (while (and (not (diffscuss-parse-leader))
              (zerop (forward-line -1))))
  (if (diffscuss-parse-leader)
      (goto-char (diffscuss-find-comment-start)))
  (recenter))

;; mailbox

(defun diffscuss-mb-check ()
  (interactive)
  (let ((outbuf (get-buffer-create "*diffscuss-mb-check*")))
    (with-current-buffer outbuf
      (setq buffer-read-only nil)
      (text-mode)
      (erase-buffer))
    (call-process
     diffscuss-exe
     nil outbuf nil
     "mailbox"
     "check"
     "-e")
    (with-current-buffer outbuf
      (goto-char (point-min))
      (let ((compilation-error-regexp-alist
             '(("\\(/.*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))
        (compilation-mode)))
    (pop-to-buffer outbuf)))

(defun diffscuss-mb-cmd-impl (recips verb cmd)
  (let ((orig-file buffer-file-name)
        (new-file buffer-file-name))
    (with-temp-buffer
      (let ((exe-res nil))
        (if (string= "" recips)
            (setq exe-res (call-process
                           diffscuss-exe
                           nil t nil
                           "mailbox"
                           cmd
                           "--print-review-path"
                           orig-file))
          (setq exe-res (apply
                         'call-process
                         diffscuss-exe
                         nil t nil
                         "mailbox"
                         cmd
                         "--print-review-path"
                         orig-file
                         (split-string recips " "))))
        (if (= 0 exe-res)
            (progn (setq new-file (diffscuss-trim-string (buffer-string)))
                   (message "%s successful" verb))
          (message "Could not %s, got error: %s" verb (buffer-string)))))
    (if (not (string= buffer-file-name new-file))
        (progn (set-visited-file-name new-file)
               (message "Visiting new review file.")))))

(defun diffscuss-mb-post (recips)
  (interactive "sEnter post recipients separated by space: ")
  (diffscuss-mb-cmd-impl recips "post" "post"))

(defun diffscuss-mb-bounce (recips)
  (interactive "sEnter bounce recipients separated by space: ")
  (diffscuss-mb-cmd-impl recips "bounce" "bounce"))

(defun diffscuss-mb-done ()
  (interactive)
  (diffscuss-mb-cmd-impl "" "mark done" "done"))

;; Define the mode.

;;;###autoload
(define-derived-mode diffscuss-mode fundamental-mode "Diffscuss"
  "Major mode for inter-diff code review."
  (setq font-lock-defaults '(diffscuss-font-lock-keywords t))
  (set (make-local-variable 'fill-paragraph-function) 'diffscuss-fill-paragraph))


(provide 'diffscuss-mode)
;;; diffscuss-mode.el ends here
