;;; setup-editing.el --- GROUP: Editing -> Editing Basics
;;; Commentary:
;;; Code:
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)


(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; Package: undo-tree
;; GROUP: Editing -> Undo -> Undo Tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))

;; Package: clean-aindent-mode
(use-package clean-aindent-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; PACKAGE: iedit
(use-package iedit
  :ensure t
  :bind (("C-;" . iedit-mode))
  :init
  (setq iedit-toggle-key-default nil))

(electric-pair-mode t)

(use-package ido
  :ensure t
  :init
  (setq ido-enable-flex-matching t)
  ;(setq ido-everywhere t)
  ;(setq ido-create-new-buffer 'always)
  :config
  (defun cbs:ido-ignore-non-user-except-scratch(name)
    "Ignore all non-user (a.k.a *starred*) buffers except *scratch*."
    (and (string-match "^\*" name)
         (not (string= name "*scratch*"))))
  (setq ido-ignore-buffers '("\\` " cbs:ido-ignore-non-user-except-scratch))
  (ido-mode t))

;; utilities

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

;; company
(use-package company
  :ensure t
  :init
  (global-company-mode t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-begin-commands '(self-insert-command))
  (global-set-key (kbd "C-c /") 'company-files))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode))


(use-package evil
  :ensure t
  :config
  (evil-mode t))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(provide 'setup-editing)
;;; setup-editing.el ends here
