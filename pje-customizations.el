;;
;; Emacs customizations I use everywhere
;;
;; To Use:
;;  (setq load-path (append load-path '("~/path/to/dir/containing/this-file")))
;;  (require 'pje-customizations)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;(when (not package-archive-contents)
;  (package-refresh-contents))

;;
;; General UI Stuff
;;

(scroll-bar-mode -1)
(tool-bar-mode -1)
(electric-indent-mode +1)
(menu-bar-mode -1)

(defun enable_flyspell ()
  (ispell-change-dictionary "american")
  (flyspell-prog-mode))

;; Cut the word currently at point
(defun cut-current-word ()
  (interactive)
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (delete-region (point) end)))

; Don't open a new, little frame for ediff 
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;
;; Desktop mode settings, some from http://www.emacswiki.org/emacs/DeskTop
;;

(require 'desktop)
;;(setq desktop-dirname "~/.emacs.d/desktop-saves/")
(desktop-save-mode 1)

(defun custom-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

;; Save desktop on idle
(add-hook 'auto-save-hook 'custom-desktop-save)

;;
;; Lisp/Slime
;;

(setq slime-lisp-implementations
      '((ccl ("/usr/local/bin/ccl64" "--quiet"))
        (sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))

;;
;; ispell/aspell
;;

;; The Following is set in init.el's since the program location varies
;; by host
;;(setq ispell-program-name "aspell")

(setq ispell-list-command "list")

;;
;; Groovy
;;

(add-to-list 'auto-mode-alist '("\\.groovy" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle" . groovy-mode))

;; JavaScript mode indent 
(setq js-indent-level 2)

;; Format Json using python library that's (typically) installed
;; by default
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; Magit mode for git
;(require 'magit)

; To automatically enter closing pair when opening pair is entered
;(electric-pair-mode +1)

;; Ruby
;; Don't indent parameters inside parens more than normal 
(setq ruby-deep-indent-paren nil)
;; enable subword (CamelCase-aware) just in ruby-mode
(add-hook 'ruby-mode-hook 'subword-mode)

;; Don't split window horizontially
(setq split-height-threshold 0)
(setq split-width-threshold nil)

(put 'narrow-to-region 'disabled nil)

;; Turn off auto-fill-mode in html mode
(add-hook 'html-mode-hook (lambda () (auto-fill-mode -1)))

;; Make tab width 2 chars
(setq default-tab-width 2)

;; Turn on column number mode
(column-number-mode)

;; Don't minimize emacs with Ctrl-Z
(global-unset-key (kbd "C-z"))

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;(global-set-key (kbd "C-M-x") 'cut-current-word)

;; Show matching parens
(show-paren-mode)

;; Allow direct edting of permission flags in wdired
(setq wdired-allow-to-change-permissions t)

;; Don't ask to delete excess versions of files
(setq trim-versions-without-asking t)

(require 'ido)
(ido-mode t)

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

;; XML pretty printer, requires nXML mode
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)

;;

(defun kill-buffers-like (pattern)
  (dolist (buffer (buffer-list))
    (if (string-match pattern (buffer-name buffer))
        (kill-buffer buffer))))

(defun kill-p4-buffers ()
  (interactive)
  (save-excursion
    (kill-buffers-like "\\*P4 Output\\*")))

;; ---------------------
;; Org Mode
;; ---------------------

;; Org Mode default org directory
(setq org-directory "~/Dropbox/org")

;;

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;(global-set-key (kbd "C-x n") 'other-window)
(global-set-key (kbd "C-x p") 'other-window-backward)

;; Note that the functions switch-to-buffer-other-window and
;; switch-to-buffer-other-frame are not currently similarlly advised:
(defadvice switch-to-buffer (before existing-buffer 
				    activate compile)
  "When interactive, switch to existing buffers only,
unless given a prefix argument. Prevents unintentionally
creating buffers."
  (interactive
   (list (read-buffer "Switch to buffer:"
		      (other-buffer)
		      (null current-prefix-arg)))))

;;
;; Unscroll support
;;

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to unscroll")
(defvar unscroll-window-start (make-marker)
  "Window start position for next call to unscroll")
(defvar unscroll-hscroll nil 
  "Hscroll position for next call to unscroll")

(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn 
        (set-marker unscroll-point (point))
        (set-marker unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))

(defadvice scroll-up (before remember-for-unscroll
                                  activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
                                    activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll
                                    activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll
                                     activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defun unscroll ()
  "Jump to location specified by 'unscroll-to'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(defvar insert-time-format "%X"
  "*Format for \\[insert-time] (c.f. 'format-time-string').")
(defvar insert-date-format "%Y/%m/%d"
  "*Format for \\[insert-date] (c.f. 'format-time-string').")

(defun insert-date ()
  "Insert the current date"
  (interactive "*")
  (insert (format-time-string insert-date-format 
                                    (current-time))))

(defun insert-time ()
  "Insert the current time"
  (interactive "*")
  (insert (format-time-string insert-time-format 
                                    (current-time))))

;;
;; Common Lisp
;;

;; Don't ask about variable settings in lisp files
(setq enable-local-variables nil)

;; ---------------------
;; OSX Clipboard stuff
;; ---------------------

(defun setup-osx-clipboard-mods ()
  ;; Don't have emacs kills to go to sytem clipboard because it defeats
  ;; the usefullness of ClipMenu
  (setq interprogram-cut-function nil)
  ;; Don't use C-y (yank) to paste from system clipboard, use Command-V instead
  (setq interprogram-paste-function nil)

  (defun paste-from-pasteboard ()
    (interactive)
    (and mark-active (filter-buffer-substring (region-beginning) (region-end) t))
    (insert (ns-get-pasteboard)))

  (defun copy-to-pasteboard (p1 p2)
    (interactive "r*")
    (ns-set-pasteboard (buffer-substring p1 p2))
    (message "Copied selection to pasteboard"))

  (defun cut-to-pasteboard (p1 p2)
    (interactive "r*") (ns-set-pasteboard (filter-buffer-substring p1 p2 t)))

  (global-set-key (kbd "s-v") 'paste-from-pasteboard)
  (global-set-key (kbd "s-c") 'copy-to-pasteboard)
  (global-set-key (kbd "s-x") 'cut-to-pasteboard))

;;
;; Window System Config
;;

(when window-system
  (blink-cursor-mode 1))

;;
;; OS-Dependant Config
;;

;; OSX
(when (eq system-type 'darwin)
  (setup-osx-clipboard-mods))

;; Start emacsserver if not yet running
;(unless (server-running-p)
;  (server-start))

; Disable emacs starter kit highlighting of current line
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(require 'term)

;; (defun visit-term-buffer ()
;;   "Create or visit a ansi terminal buffer."
;;   (interactive)
;;   (if (not (get-buffer "*ansi-term*"))
;;       (progn
;;        (split-window-sensibly (selected-window))
;;        (other-window 1)
;;        (ansi-term (getenv "SHELL")))
;;       (switch-to-buffer-other-window "*ansi-term*")))

;(global-set-key (kbd "C-c t") 'visit-term-buffer)

(defun visit-ansi-term ()
    "If the current buffer is:
       1) a running ansi-term named *ansi-term*, rename it.
       2) a stopped ansi-term, kill it and create a new one.
       3) a non ansi-term, go to an already running ansi-term
          or start a new one while killing a defunt one"
    (interactive)
    (let ((is-term (string= "term-mode" major-mode))
          (is-running (term-check-proc (buffer-name)))
          (term-cmd (getenv "SHELL"))
          (anon-term (get-buffer "*ansi-term*")))
      (if is-term
          (if is-running
              (if (string= "*ansi-term*" (buffer-name))
                  (call-interactively 'rename-buffer)
                (if anon-term
                    (switch-to-buffer "*ansi-term*")
                  (ansi-term term-cmd)))
            (kill-buffer (buffer-name))
            (ansi-term term-cmd))
        (if anon-term
            (if (term-check-proc "*ansi-term*")
                (switch-to-buffer "*ansi-term*")
              (kill-buffer "*ansi-term*")
              (ansi-term term-cmd))
          (ansi-term term-cmd)))))

(global-set-key (kbd "C-c t") 'visit-ansi-term)

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
         (read-string "Google: "))))))

(global-set-key (kbd "C-c g") 'google)

(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(global-set-key (kbd "C-c o") 'open-with)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting, remove from
version control if file is under version control."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(global-set-key (kbd "C-M-z") 'indent-defun)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(setq quote-xml
   [?\C-s ?< ?\C-m ?\C-b ?\" ?\C-e ?\" ?\S-  ?+ ?\C-a ?\C-n])

;; Java mode

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2
                  arglist-intro 2
                  tab-width 2
                  indent-tabs-mode nil
                  arglist-cont 2
                  arglist-cont-nonempty 2 ; Indent wrapped argument lists by 2
                  substatement-open 0)    ; Don't indent opening braces on their own line
            (local-set-key (kbd "RET") 'newline-and-indent)
            ;; Treat Java 1.5 @-style annotations as comments.
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;(add-hook 'java-mode-hook 'my-java-mode-hook)
;(add-hook 'java-mode-hook 'subword-mode)

;; Lisp Mode

(defun my-lisp-mode-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

;; Emacs Lisp Mode

(defun my-elisp-mode-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'lisp-mode-hook 'my-elisp-mode-hook)

(provide 'pje-customizations)

