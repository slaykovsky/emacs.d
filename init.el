;;; package --- Summary
;;; Commentary:
;;; Use it on your own risk :)
;;; Code:

(setq gc-cons-threshold 64000000)
(add-hook
 'after-init-hook #'(lambda ()
                      (setq gc-cons-threshold 800000)))

;;; global vars
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 sentence-end-double-space nil
 select-enable-clipboard t
 select-enable-primary t
 ring-bell-function 'ignore
 max-lisp-eval-depth most-positive-fixnum
 browse-url-browser-function 'browse-url-firefox
 max-specpdl-size most-positive-fixnum)

;;; buffer local variables
(setq-default
 indent-tabs-mode nil
 show-parn-delay 0.5
 tab-width 4
 c-basic-offset 4)

;;; package management
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))

 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-install 'use-package))

(package-refresh-contents)

(eval-when-compile
  (require 'use-package))
(setq  use-package-always-ensure t)

(require 'bind-key)
(require 'diminish)

(use-package erc)
(use-package quick-peek)
(use-package magit
  :bind ("C-c s" . magit-status))
(use-package python-mode)
(use-package dockerfile-mode)
(use-package base16-theme)
(use-package bind-key)
(use-package dash)
(use-package dash-functional)
(use-package company)
(use-package smartparens)
(use-package org)
(use-package popwin)
(use-package s)
(use-package f)
(use-package expand-region)
(use-package markdown-mode+)
(use-package enh-ruby-mode)
(use-package org)
(use-package yaml-mode)
(use-package counsel)
(use-package ivy)
(use-package swiper)
(use-package rainbow-delimiters)
(use-package aggressive-indent)
(use-package intero)
(use-package hasky-stack)
(use-package rust-mode)
(use-package flycheck-rust)
(use-package protobuf-mode)
(use-package anaconda-mode)
(use-package company-anaconda)
(use-package python-docstring)
(use-package py-isort)
(use-package diminish)
(use-package pip-requirements)
(use-package importmagic
  :diminish importmagic-mode
  :config
  (add-hook 'python-mode-hook 'importmagic-mode))
(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode))
(use-package base16-theme)

(require 'smartparens-config)

;;; modes
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(electric-indent-mode 0)
(global-hl-line-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(set-default 'truncate-lines t)
(show-paren-mode 't)
(global-visual-line-mode 't)
(global-flycheck-mode 't)
(ivy-mode t)
(desktop-save-mode 1)
(global-auto-revert-mode 1)

(setq
 message-kill-buffer-on-exit t
 ivy-use-virtual-buffers t)

;;; global keybindings
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-#") 'comment-or-uncomment-region)

;;; UI related stuff
(load-theme 'base16-3024 t)
(set-frame-font "Iosevka 14")

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . enh-ruby-mode))

;; Python
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook 'python-docstring-mode)
(add-hook 'before-save-hook 'py-isort-before-save)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; Rust
(require 'rust-mode)
(defun set-newline-and-indent ()
  "A function to bind autoindentation in rust mode."
  (local-set-key (kbd "RET") 'newline-and-indent))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'set-newline-and-indent))
(setq rust-format-on-save t)

;; Haskell
(require 'hasky-stack)
(add-hook 'haskell-mode-hook 'intero-mode)

;; Lisps
(defvar lisp-modes "clojure emacs-lisp cider-repl")

(defun standard-lisp-environment ()
  "A collection of modes to be enabled for a LISP."
  (subword-mode 1)
  (smartparens-mode 1)
  (rainbow-delimiters-mode 1)
  (aggressive-indent-mode 1)
  (eldoc-mode 1))

(defun append-suffix (suffix phrases)
  "Take SUFFIX and append it to each of the PHRASES."
  (mapcar #'(lambda (phrase) (concat phrase suffix)) phrases))

(defun multiple-mode-add-hook (modes hook)
  "Given a list of x-mode-hook symbols in MODES, add the HOOK to them."
  (mapc (lambda (mode) (add-hook mode hook)) modes))

(defun symbols-from-strings (strings)
  "Given a list of STRINGS, get their symbol values."
  (mapcar #'intern strings))

(defun hook-up-modes (string hook)
  "Using mode string STRING add a hook HOOK to it."
  (let ((modes (symbols-from-strings
                (append-suffix "-mode-hook" (split-string string)))))
    (multiple-mode-add-hook modes hook)))

(hook-up-modes lisp-modes 'standard-lisp-environment)

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

;;; ERC
(require 'erc)
(require 'erc-log)

(load "~/.ercpass")
(require 'erc-services)
(setq erc-nickserv-identify-mode 'autodetect)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      '((freenode (("slaykovsky" . freenode-pass)))))

(require 'erc-join)

(require 'erc-desktop-notifications)
(require 'erc-button)
(setq
 erc-autojoin-channels-alist '(("freenode.net" "#lor"))
 erc-autojoin-timing 'ident
 erc-notifications-bus :session
 erc-notifications-mode t
 erc-email-userid "alexey@slaykovsky.com")
(erc-autojoin-mode t)

(require 'erc-track)
(erc-track-mode t)
(setq erc-track-exclude-types
      '("JOIN" "NICK" "PART" "QUIT" "MODE"
        "324" "329" "332" "333" "353" "477"))

(defun erc-start-or-switch ()
  "Start ERC or switch to the last active one."
  (interactive)
  (if (get-buffer "chat.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (erc :server "chat.freenode.net"
         :port 6667
         :nick "LeakyReLU")))

(provide 'init)
;;; init.el ends here
