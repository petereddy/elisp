(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode t)
(setq visible-bell t)
(column-number-mode)
;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; Use left alt/option key for meta and the right for normal alt/option functionality
(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;; ispell
(setq-default ispell-program-name "/opt/homebrew/bin/aspell")

;; emacs server
(server-start)

;; Save/restore desktop when exiting/starting emacs
(desktop-save-mode 1)

;; Use transparent titlebar
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; Use GNU ls on macOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))


;; Set up package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work")
    (setq projectile-project-search-path '("~/work")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package rainbow-delimiters
  :hook (proj-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq wich-key-idel-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package elixir-ts-mode
  :ensure t)

;;;
;;; Utility Functions
;;;

(defun tidy-html ()
  "Tidies the HTML content in the buffer using `tidy'"
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "tidy -i -w 120 -q"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Tidy Error Buffer*"
   ;; show error buffer?
   t))

(defun os-open (arg)
  "Open the current file or directory with the OS's perferred
application. With a prefix argument (C-n), always open the
containing directory."
  (interactive "P")
  (when-let (path (or
                   (and arg (or
                             default-directory
                             (dired-current-directory)))		   
                   (dired-get-filename nil t)
                   (buffer-file-name
                    (window-buffer
                     (minibuffer-selected-window)))
                   default-directory))
    (shell-command (concat "open" " " path))))

(global-set-key (kbd "C-s-o") 'os-open)

;;;
;;; CSV Mode
;;;

(use-package csv-mode)

(require 'color)

(defun csv-highlight (&optional separator)
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
         (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                       collect (apply #'color-rgb-to-hex 
                                      (color-hsl-to-rgb i 0.3 0.5)))))
    (loop for i from 2 to n by 2 
          for c in colors
          for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
          do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))


(add-hook 'csv-mode-hook 'csv-highlight)
(add-hook 'csv-mode-hook 'csv-align-mode)
(add-hook 'csv-mode-hook '(lambda () (interactive)
                            (toggle-truncate-lines nil)))

(defun os-open (arg)
  "Open the current file or directory with the OS's perferred
application. With a prefix argument (C-n), always open the
containing directory."
  (interactive "P")
  (when-let (path (or
                   (and arg (or
                             default-directory
                             (dired-current-directory)))		   
                   (dired-get-filename nil t)
                   (buffer-file-name
                    (window-buffer
                     (minibuffer-selected-window)))
                   default-directory))
    (shell-command (concat "open" " " path))))

(global-set-key (kbd "C-s-o") 'os-open)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elixir-ls elixir-mode csv-mode counsel ivy-rich which-key rainbow-delimiters magit projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
