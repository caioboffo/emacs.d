;;; setup-c.el --- Configure c programming preferences

;;; Commentary:

;;; Code:

(use-package rtags
  :ensure t
  :config
  (setq rtags-completions-enabled t))

(use-package helm-rtags
  :ensure t
  :config
  ;; comment this out if you don't have or don't use helm
  (setq rtags-display-result-backend 'helm))

(use-package company-rtags
  :ensure t
  :config
  (define-key c-mode-base-map (kbd "M-.")
	(function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
	(function rtags-find-references-at-point))
  (define-key c-mode-base-map (kbd "M-]")
    (function rtags-find-virtuals-at-point))
  
  (push 'company-rtags company-backends)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete)))


(use-package flycheck-rtags
  :ensure t)

(provide 'setup-rtags)
;;; setup-editing.el ends here
