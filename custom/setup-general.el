;;; setup-general.el --- Essential settings.
;;-*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Turn visualization of matching parens
(show-paren-mode 1)

;; Turn highlight line mode
(global-hl-line-mode t)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq-default left-fringe-width nil)

;; show empt lines after the buffer end
(setq-default indicate-empty-lines t)

;; Enable long files to be loaded
(setq large-file-warning-threshold nil)

;; prevents windows to split horizontally
(setq split-width-threshold nil)

;; show column number at the powerline bar
(column-number-mode t)

;; Fill-column easy reformat code
(setq-default fill-column 80)

;; Allow confusing functions
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Move backups to backups folder
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-default nil)

;; Allow answers with y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; apply defaul size to windows
;;(setq default-frame-alist '((width . 82) (height . 24)))

;; change apearance Font and Theme
(load-theme 'monokai t)

(scroll-bar-mode -1)

;; Enable SemanticDb
;;(semantic-mode 1)

;; Package zygospore
(use-package zygospore
  :ensure t
  :bind ("C-x 0" . zygospore-toggle-delete-other-windows))

;; Neo Tree
(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle))

(use-package ace-window
  :ensure t
  :bind  ("C-x o" . ace-window))


(defun sort-lines-nocase ()
  "Sort lines ignoring the case."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;; Setting capture a tool to track TODO items
;; (setq org-default-notes-file (concat org-directory "/notes/todo.org"))


(provide 'setup-general)
;;; setup-general ends here
