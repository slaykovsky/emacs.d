;;; package --- Summary
;;; Commentary:
;;; Use it on your own risk :)
;;; Code:
(defvar browse-url-generic-program)
(defvar display-line-numbers)
(defvar elpy-rpc-backend)
(defvar elpy-rpc-large-buffer-size)
(defvar gofmt-command)
(defvar ido-enable-flex-matching)
(defvar ido-use-filename-at-point)
(defvar ido-auto-merge-work-directories-length)
(defvar ido-use-virtual-buffers)
(defvar rainbow-delimiters-mode)
(defvar recentf-max-menu-items)
(defvar recentf-save-file)
(defvar save-place-file)
(defvar smex-save-file)

(setq display-line-numbers 't)

(define-globalized-minor-mode global-rainbow-delimiters-mode
  rainbow-delimiters-mode
  (lambda () (rainbow-delimiters-mode 1)))

(let ((gc-cons-threshold most-positive-fixnum))
  (remove-hook 'find-file-hooks 'vc-find-file-hook)

  (setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("elpy" . "https://jorgenschaefer.github.io/packages/")))
  (require 'package)
  (package-initialize)
  (desktop-save-mode 1)
  (setq inhibit-startup-message t)
  (set-default 'truncate-lines t)

  (require 'cask "~/.cask/cask.el")
  (cask-initialize)

  (setq scroll-margin 1
	scroll-step 1
	scroll-conservatively 10000
	scroll-preserve-screen-position 1)

  ;;; UI related stuff
  (load-theme 'gruvbox t)
  
  (set-frame-font "PragmataPro for Powerline 12")
  
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  
  (show-paren-mode 't)
  (global-visual-line-mode 't)
  (global-flycheck-mode 't)
  
  (setq select-enable-clipboard t
	select-enable-primary t
	save-interprogram-paste-before-kill t
	mouse-yank-at-point t)
  (setq ring-bell-function 'ignore)

  (global-hl-line-mode t)

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  ;;; Rainbow for all!!!
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
  (setq elpy-rpc-large-buffer-size 999999999999)

  ;;; Haskell related stuff
  (autoload 'ghc-init "ghc" nil 't)
  (autoload 'ghc-debug "ghc" nil 't)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  
  ;; Golang
  (defvar local-gopath (concat (getenv "HOME") "/Documents/go"))
  (setenv "GOPATH" local-gopath)
  (add-to-list 'exec-path
	       (concat local-gopath "/bin"))
  (add-to-list 'load-path
	       (concat local-gopath "/src/github.com/dougm/goflymake"))
  (add-to-list 'load-path
	       (concat local-gopath "/src/github.com/dougm/goflymake"))
  (require 'go-flymake)
  (require 'go-flycheck)

  (defun my-go-mode-hook ()
    "Override some go-mode."
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (if (not (string-match "go" compile-command))
	(set (make-local-variable 'compile-command)
	     "go build -v && go test -v && go vet"))
    (go-guru-hl-identifier-mode)
    
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (local-set-key (kbd "M-p") 'compile)
    (local-set-key (kbd "M-P") 'recompile)
    (local-set-key (kbd "M-]") 'next-error)
    (local-set-key (kbd "M-[") 'previous-error)

    (auto-complete-mode 1))
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (with-eval-after-load 'go-mode
    (require 'go-autocomplete))
  (require 'go-guru)

  ;; Editing :)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (setq recentf-max-menu-items 10)

  (ido-mode t)
  (setq ido-enable-flex-matching 't)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t)
  (ido-ubiquitous-mode 1)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;;; Smex
  (autoload 'smex "smex"
    "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")

  (global-set-key (kbd "M-x") 'smex)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))

  (projectile-mode)

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
	browse-url-generic-program "chromium")

  ;;; Custom VERY useful functions ^_^
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


  (nyan-mode t)
  (nyan-start-animation))
(provide 'init)
;;; init.el ends here

