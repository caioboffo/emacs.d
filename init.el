;;; init.el -- My Emacs configuration
;;-*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.local/bin")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Standard package repositories
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable"  . "https://stable.melpa.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

(require 'dired-x)

;;; General Settings

(require 'setup-general)
(require 'hide-comnt)
(require 'setup-editing)
(require 'setup-rtags)
(require 'setup-powerline)
(require 'setup-c)
(require 'setup-helm)
(require 'init-python)
(provide 'init)
;;; init.el ends here
