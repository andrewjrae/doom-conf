;;; +sidefx.el -*- lexical-binding: t; -*-

;; SideFX syntax
(add-hook! 'c-mode-common-hook
  (unsetq-hook! 'after-change-major-mode-hook evil-shift-width)
  (setq tab-width 8)
  (setq c-tab-always-indent nil)
  (setq evil-shift-width 4)
  (map! :leader
        ;; :desc "Compile" "c c" '(shell-command "mi")
        :desc "Toggle between header and source file" "b o" 'ff-find-other-file))

;; Shortcut for project wide diff
(map! :leader
      :desc "View project diff with version control" "p v" 'vc-root-diff)

;; Basic syntax highlighting for VEX
;; (require 'vfl-mode)
(use-package! vfl-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.vfl\\'" . vfl-mode))
  (add-hook! 'vfl-mode-hook
    (unsetq-hook! 'after-change-major-mode-hook evil-shift-width)
    (setq tab-width 8)
    (setq evil-shift-width 4)))

;; HACK: super ghetto disabling of dap-ui-controls-mode for TUI use
(define-minor-mode dap-ui-controls-mode
  "Displaying DAP visuals."
  :init-value nil
  :global t
  :require 'dap-ui)
;; less hack solution to disabling dap-ui-controls-mode but it doesn't fully work
(after! dap-mode
  (setq dap-auto-configure-features (remove 'controls dap-auto-configure-features)))

(use-package! usda-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.usda\\'" . usda-mode)))

(require 'xclip)
;; xclip mode seems to slow down pasting so just leave it off by default
;; (xclip-mode 1)
