;;; package --- Summary
;;; Commentary:
;;; Use it on your own risk :)
;;; Code:

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

(require 'use-package)

(use-package gruvbox-theme)
(use-package bind-key)
(use-package dash)
(use-package company)
(use-package flycheck)
(use-package magit)
(use-package web-mode)
(use-package yaml-mode)
(use-package elpy)
(use-package python-mode)
(use-package counsel)
(use-package ivy)
(use-package idris-mode)
(use-package haskell-mode)
(use-package ensime)
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
(use-package dockerfile-mode)
(use-package vagrant)
(use-package popwin)
(use-package s)
(use-package f)
(use-package expand-region)
(use-package markdown-mode+)
(use-package org)

;;; global vars
(setq
 browse-url-browser-function 'browse-url-chromium
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 message-kill-buffer-on-exit t
 scroll-error-top-bottom t
 use-package-always-ensure t
 sentence-end-double-space nil
 select-enable-clipboard t
 select-enable-primary t
 ring-bell-function 'ignore
 max-lisp-eval-depth most-positive-fixnum
 max-specpdl-size most-positive-fixnum)

;;; buffer local variables
(setq-default
 indent-tabs-mode nil
 show-parn-delay 0.5
 tab-width 4
 c-basic-offset 4)

;;; modes
(global-display-line-numbers-mode)
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(electric-indent-mode 0)
(global-hl-line-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(set-default 'truncate-lines t)
(show-paren-mode 't)
(global-visual-line-mode 't)
(global-flycheck-mode 't)
(ivy-mode t)
(setq
 ivy-use-virtual-buffers t)
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
(load-theme 'gruvbox t)
(set-frame-font "Hack 9")

;;; Python
(elpy-enable)
(setq
 elpy-rpc-backend "jedi"
 elpy-rpc-large-buffer-size most-positive-fixnum)

;;; Haskell
(add-hook 'haskell-mode-hook
	      (lambda ()
	        (set (make-local-variable 'company-backends)
		         (append '((company-capf company-dabbrev-code))
			             company-backends))))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;;; Idris
(setq idris-interpreter-path "/home/aslaikov/.cabal/bin/idris")

;;; Scala
(setq
 scala-indent:default-run-on-strategy "eager"
 scala-indent:indent-value-expression t
 scala-indent:align-parameters t
 scala-indent:align-forms t
 prettify-symbols-alist scala-prettify-symbols-alist
 ensime-startup-notification nil)
(prettify-symbols-mode)

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

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
