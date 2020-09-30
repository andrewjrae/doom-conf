;;; +bindings.el -*- lexical-binding: t; -*-

;; File for all my general bindings

;; Adds Ctrl-S for saving / exiting vim modes, cause I just can't break that habit ;)
(map! :n "C-s" #'save-buffer
      :ivr "C-s" (lambda () (interactive) (evil-normal-state) (save-buffer)))
;; Better search and replace
(map! :leader
      :desc "Search and replace" "s r" #'query-replace-regexp)
;; Remapping the old SPC-s-r
(map! :leader
      :desc "Jump to mark" "s m" #'counsel-mark-ring)
(map! :leader
      :desc "Jump to bookmark" "s M" #'bookmark-jump)
;; Better switching between workspaces
(map! :leader
      :desc "Switch to last workspace" "TAB l" #'+workspace/other)
(map! :leader
      :desc "Load workspace from file" "TAB L" #'+workspace/other)
;; Drag text with meta (for some reason evil didn't cover this)
(map! :n "M-h" #'drag-stuff-left
      :n "M-j" #'drag-stuff-down
      :n "M-k" #'drag-stuff-up
      :n "M-l" #'drag-stuff-right
      ;; Replace the old M-j
      :n "C-j" #'newline-and-indent)

;; Make movement keys work on visual lines
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; DAP mode bindings
(after! dap-mode
  (map! :leader
        (:prefix-map ("d" . "debug")
         :desc "Start session" "d" #'dap-debug
         :desc "Switch session" "s" #'dap-switch-session
         :desc "Kill session" "k" #'dap-delete-session
         :desc "Hydra" "h" #'dap-hydra
         :desc "Continue" "c" #'dap-continue
         :desc "Restart" "r" #'dap-debug-restart
         (:prefix-map ("e" . "evaluate")
          :desc "Evaluate" "e" #'dap-eval
          :desc "Evaluate region" "r" #'dap-eval-region
          :desc "Evaluate at point" "i" #'dap-eval-thing-at-point)
         :desc "Add watch" "a" #'dap-ui-expressions-add-prompt
         :desc "Remove watch" "D" #'dap-ui-expressions-remove
         :desc "Edit template" "t" #'dap-debug-edit-template
         :desc "List breakpoints" "l" #'dap-hydra/dap-ui-breakpoints
         :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle)))
