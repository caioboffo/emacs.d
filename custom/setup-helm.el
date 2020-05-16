;;; setup-helm.el --- Helm config
;;; Commentary:
;;; Code:
;;; Enable Modes (helm-mode is loading nearly everything).
;;
(use-package helm-mode
  :init
  (add-hook 'helm-mode-hook
            (lambda ()
              (setq completion-styles
                    (cond ((assq 'helm-flex completion-styles-alist)
                           '(helm-flex))
                          ((assq 'flex completion-styles-alist)
                           '(flex))))))
  :config
  ;;; Global-map
  ;;
  ;;
  (global-set-key (kbd "M-x")                          'undefined)
  (global-set-key (kbd "M-x")                          'helm-M-x)
  (global-set-key (kbd "M-y")                          'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f")                      'helm-find-files)
  (global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
  (global-set-key [remap bookmark-jump]                'helm-filtered-bookmarks)
  (global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
  (global-set-key (kbd "C-,")                          'helm-calcul-expression)
  (global-set-key (kbd "C-h d")                        'helm-info-at-point)
  (global-set-key (kbd "C-h i")                        'helm-info)
  (global-set-key (kbd "C-x C-d")                      'helm-browse-project)
  (global-set-key (kbd "<f1>")                         'helm-resume)
  (global-set-key (kbd "C-h C-f")                      'helm-apropos)
  (global-set-key (kbd "C-h a")                        'helm-apropos)
  (global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
  (global-set-key (kbd "<f5> s")                       'helm-find)
  (global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
  (global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
  (global-set-key (kbd "C-c C-i")                      'helm-imenu)
  (global-set-key (kbd "<f11>")                        nil)
  (global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
  (global-set-key (kbd "C-s")                          'helm-occur)
  (global-set-key (kbd "<f6> h")                       'helm-emms)
  (define-key global-map [remap jump-to-register]      'helm-register)
  (define-key global-map [remap list-buffers]          'helm-mini)
  (define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
  (define-key global-map [remap find-tag]              'helm-etags-select)
  (define-key global-map [remap xref-find-definitions] 'helm-etags-select)
  (define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
  (define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
  (define-key global-map (kbd "M-g i")                 'helm-gid)
  (define-key global-map (kbd "C-x r p")               'helm-projects-history)
  (define-key global-map (kbd "C-x r c")               'helm-addressbook-bookmarks)
  (define-key global-map (kbd "C-c t r")               'helm-dictionary)
  (define-key helm-map (kbd "<tab>")                   'helm-execute-persistent-action)
  (helm-mode 1))

(use-package helm-adaptive
  :config
  (setq helm-adaptive-history-file nil)
  (helm-adaptive-mode 1))

(use-package helm-sys
  :commands (helm-top)
  :config (helm-top-poll-mode 1))

(use-package helm-info
  :bind ("C-h r" . helm-info-emacs))

(use-package helm-ring
  :config
  ;; Action for helm kill-ring
  (defun helm/emamux:copy-from-kill-ring (candidate)
    (require 'emamux)
    (emamux:check-tmux-running)
    (when (null kill-ring)
      (error "kill-ring is nil!!"))
    (emamux:set-buffer candidate 0))
  (add-to-list 'helm-kill-ring-actions '("Emamux copy" . helm/emamux:copy-from-kill-ring) t)
  :bind (:map helm-kill-ring-map
              ("C-d" . helm-kill-ring-run-persistent-delete)))


(provide 'setup-helm)
