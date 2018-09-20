;;; package --- Elpy install and config
;;; Commentary: This is python elpy for emacs config
;;; Code:

;;; apt install elpa-elpy

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  ;;(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc--backend-python-command "python3")
  (setq python-shell-interpreter "python3")
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(provide 'init-python)

;;; init-python.el ends here
