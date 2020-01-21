;;; setup-c.el --- Configure c programming preferences

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq-default c-default-style "linux"
      tab-width 4
      c-basic-offset 4
      indent-tabs-mode nil)

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (defun cbs:my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  ;;(add-hook 'irony-mode-hook 'cbs:my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package company-c-headers
  :ensure t
  :init
  (add-to-list 'company-backends 'company-c-headers))

(use-package clang-format
  :ensure t
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (when (member major-mode '(c-mode c++-mode))
                (progn
                  (when (locate-dominating-file "." ".clang-format")
                    (clang-format-buffer))
                  ;; return nil to continue saving
                  nil)))))

(provide 'setup-c)
;;; setup-c.el ends here
