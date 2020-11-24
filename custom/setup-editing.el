;;; setup-editing.el --- GROUP: Editing -> Editing Basics
;;; Commentary:
;;; Code:
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)

(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; duplicate line command
(global-set-key "\C-d" "\C-a\C- \C-n\M-w\C-y\C-p")

(defun move-text-internal (arg)
  "Move region up or down depending on ARG."
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [(meta up)] 'move-text-up)
(global-set-key [(meta down)] 'move-text-down)

;; set truncate-lines global
(setq-default truncate-lines t)

;; Package: undo-tree
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
  :bind ("C-c ;" . iedit-mode))

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
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (flycheck-add-next-checker 'c/c++-clang 'irony)
  (flycheck-add-next-checker 'irony 'rtags))

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
  (setq company-idle-delay 0)
  (setq company-begin-commands '(self-insert-command))
  :config
  (global-set-key (kbd "<C-M-SPC>") 'company-complete-common))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (define-key magit-mode-map
    (kbd "q")
    (lambda () (interactive (magit-mode-bury-buffer t))))
  (setq magit-blame-styles
        '((headings
           (heading-format   . "%-20a %C %s\n"))
          (margin
           (margin-format    . (" %s%f" " %C %a" " %H"))
           (margin-width     . 42)
           (margin-face      . magit-blame-margin)
           (margin-body-face . (magit-blame-dimmed)))
          (highlight
           (highlight-face   . magit-blame-highlight))
          (lines
           (show-lines     . t)
           (show-message   . t))))

  ;; magit diff colors
  (custom-reset-faces
   '(magit-diff-added '((t (:background "face-background 'default" :foreground "green"))))
   '(magit-diff-added-highlight '((t (:background "face-background 'default" :foreground "green"))))
   '(magit-diff-removed '((t (:background "face-background 'default" :foreground "red"))))
   '(magit-diff-removed-highlight '((t (:background "face-background 'default" :foreground "red")))))

  )

(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode))

;; Editing assembly code
(defun my-asm-mode-hook ()
  "My asm mode hook."
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; (local-unset-key "<return>") ; doesn't work. "RET" in a terminal.  http://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
  (electric-indent-local-mode)  ; toggle off
;  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  ;; (setq tab-always-indent (default-value 'tab-always-indent))

  (defun asm-calculate-indentation ()
  (or
   ;; Flush labels to the left margin.
;   (and (looking-at "\\(\\.\\|\\sw\\|\\s_\\)+:") 0)
   (and (looking-at "[.@_[:word:]]+:") 0)
   ;; Same thing for `;;;' comments.
   (and (looking-at "\\s<\\s<\\s<") 0)
   ;; %if nasm macro stuff goes to the left margin
   (and (looking-at "%") 0)
   (and (looking-at "c?global\\|section\\|default\\|align\\|INIT_..X") 0)
   ;; Simple `;' comments go to the comment-column
   ;(and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
   ;; The rest goes at column 4
   (or 4)))
  )

(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;; Evil Numbers
;; works like C-a/C-x in vim
(use-package evil-numbers
  :ensure t
  :config
  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package embrace
  :ensure t
  :config
  (global-set-key (kbd "C-c a") 'embrace-add)
  (global-set-key (kbd "C-c c") 'embrace-change)
  (global-set-key (kbd "C-c d") 'embrace-delete))

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-default-exec-mode 'jar
        plantuml-indent-level 2))

(use-package linum-relative
  :ensure t)

;; (use-package evil
;;   :ensure t
;;   :config
;;   (evil-mode t))


(provide 'setup-editing)
;;; setup-editing.el ends here
