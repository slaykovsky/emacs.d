(setq inhibit-startup-message t)
(set-default 'truncate-lines t)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("elpy" . "https://jorgenschaefer.github.io/packages/")))
(require 'package)
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Rust things
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; UI
(load-theme 'solarized-dark t)
(global-linum-mode)
(set-frame-font "Iosevka Term 12")
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq fill-column 79)
(blink-cursor-mode -1)
(show-paren-mode t)
(global-visual-line-mode t)
(global-flycheck-mode 1)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)
(setq ring-bell-function 'ignore)

(require 'fill-column-indicator)
(setq-default fci-rule-column 79)
(setq fci-rule-use-dashes 'true)
(setq fci-handle-truncate-lines nil)
(define-globalized-minor-mode global-fci-mode
  fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)
(defun auto-fci-mode (&optional unused)
  "Enables fci-mode globally.
UNUSED is unused though."
  (if (> (window-width) fci-rule-column)
      (fci-mode 1)
    (fci-mode 0)))
(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
(add-hook 'window-configuration-change-hook 'auto-fci-mode)

(global-hl-line-mode t)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-globalized-minor-mode global-rainbow-delimiters-mode
  rainbow-delimiters-mode
  (lambda () (rainbow-delimiters-mode 1)))
(global-rainbow-delimiters-mode 1)

;; Lisp
(autoload 'enable-paredit-mode "paredit")
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lip-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Python
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command "python2")

;; Golang
(defvar local-gopath (concat (getenv "HOME") "/Documents/go"))
(setenv "GOPATH" local-gopath)
(add-to-list 'exec-path (concat local-gopath "/bin"))
(add-to-list 'load-path (concat local-gopath "/src/github.com/dougm/goflymake"))
(require 'go-flymake)
(require 'go-flycheck)

(defun my-go-mode-hook ()
  "Override some go-mode."
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
	   "go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Editing :)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq recenf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 10)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(ido-ubiquitous-mode 1)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(projectile-global-mode)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-default nil)

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(auto-revert-mode t)
(global-set-key [f1] 'eshell)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; Custom VERY useful functions ^_^
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
if there is no region, the current line will be duplicated.
However, if there's a region, all lines that region covers
will be duplicated"
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
	(exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
	(exchange-point-and-mark))
    (setq end (line-end-position))
    (if mark-active
	(exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
	(goto-char end)
	(newline)
	(beginning-of-visual-line)
	(insert region)
	(setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "M-d") 'duplicate-current-line-or-region)

(defun toggle-comment-on-line ()
  "Comments or uncomments current line."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-#") 'comment-or-uncomment-region)

(defun die-tabs()
  "Tabs to 2 spaces."
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(defun cider-run-http-server ()
  "Make cider to run http server."
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval
     (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval
     (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  "Refreshes code."
  (interactive)
  (cider-interactive-eval
   (format "(user/reset)")))

(defun cider-user-ns ()
  "Set 'user' namespace."
  (interactive)
  (cider-repl-set-ns "user"))
