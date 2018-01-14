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

(defvar prelude-packages
  '(solarized-theme bind-key dash company flycheck intero
                    magit web-mode yaml-mode elpy python-mode
                    counsel ivy idris-mode haskell-mode dockerfile-mode
                    vagrant popwin s f expand-region markdown-mode+
                    org smartparens rainbow-delimiters aggressive-indent)
  "List of packages.")

(defun prelude-packages-installed-p ()
  (cl-loop for p in prelude-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

(unless (prelude-packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(require 'smartparens-config)

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
(when (> emacs-major-version 25)
  (global-display-line-numbers-mode))
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
(load-theme 'solarized-dark t)

;;; Python
(elpy-enable)
(setq
 elpy-rpc-backend "jedi"
 elpy-rpc-large-buffer-size most-positive-fixnum)
(add-hook 'python-mode-hook #'smartparens-mode)

;;; Haskell
(add-hook 'haskell-mode-hook
	      (lambda ()
	        (set (make-local-variable 'company-backends)
		         (append '((company-capf company-dabbrev-code))
			             company-backends))))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

;;; Idris
(setq idris-interpreter-path "/home/aslaikov/.cabal/bin/idris")

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

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
