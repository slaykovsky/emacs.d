;;; package --- Summary
;;; Commentary:
;;; Use it on your own risk :)
;;; Code:

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;;; global vars
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 use-package-always-ensure t
 sentence-end-double-space nil
 select-enable-clipboard t
 select-enable-primary t
 ring-bell-function 'ignore
 max-lisp-eval-depth most-positive-fixnum
 browse-url-browser-function 'browse-url-chromium
 message-kill-buffer-on-exit t
 ivy-use-virtual-buffers t
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
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

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
(use-package flycheck)
(use-package yaml-mode)
(use-package counsel)
(use-package ivy)
(use-package swiper)
(use-package rainbow-delimiters)
(use-package aggressive-indent)

(use-package intero)

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
(use-package autorevert
  :init (global-auto-revert-mode))

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
(load-theme 'base16-gruvbox-dark-soft t)
(set-frame-font "Monospace 12")

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . enh-ruby-mode))

;; Python
;; Python
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook 'python-docstring-mode)
(add-hook 'before-save-hook 'py-isort-before-save)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))


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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#101010" "#7c7c7c" "#8e8e8e" "#a0a0a0" "#686868" "#747474" "#686868" "#b9b9b9"])
 '(ansi-term-color-vector
   [unspecified "#101010" "#7c7c7c" "#8e8e8e" "#a0a0a0" "#686868" "#747474" "#686868" "#b9b9b9"] t)
 '(custom-safe-themes
   '("c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "f869a5d068a371532c82027cdf1feefdc5768757c78c48a7e0177e90651503ad" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" default))
 '(package-selected-packages
   '(fundamental fundamental-mode groovy-mode jenkins importmagic pip-requirements py-isort python-docstring company-anaconda anaconda-mode clojure-mode elpy dash-functional use-package yaml-mode web-mode sublimity solarized-theme smartparens rainbow-delimiters python-mode popwin pdf-tools nyan-mode markdown-mode+ magit intero idris-mode hindent gotham-theme f expand-region enh-ruby-mode dracula-theme dockerfile-mode counsel-projectile color-theme bind-key bbdb- base16-theme aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
