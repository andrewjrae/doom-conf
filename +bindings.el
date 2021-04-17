;;; +bindings.el -*- lexical-binding: t; -*-

;; File for all my general bindings

;; Adds Ctrl-S for saving / exiting vim modes, cause I just can't break that habit ;)
(map! :n "C-s" #'save-buffer
      :ivr "C-s" (lambda () (interactive) (evil-normal-state) (save-buffer)))
(add-hook! 'company-mode-hook
  (map! :map company-active-map
        :g "C-s" (lambda () (interactive) (company-abort) (evil-normal-state) (save-buffer))))
(map! :leader
      :desc "Jump to mark" "s m" #'counsel-evil-marks
      :desc "Jump to bookmark" "s M" #'bookmark-jump
      :desc "Compile" "c c" #'+ivy/project-compile
      ;; Better switching between workspaces
      :desc "Switch to last workspace" "TAB l" #'+workspace/other
      :desc "Load workspace from file" "TAB L" #'+workspace/other)
;; Drag text with meta (for some reason evil didn't cover this)
(map! :n "M-h" #'drag-stuff-left
      :n "M-j" #'drag-stuff-down
      :n "M-k" #'drag-stuff-up
      :n "M-l" #'drag-stuff-right
      ;; Replace the old M-j
      :n "C-j" #'newline-and-indent)
;; My xmonad like window nav
(map! :g "C-s-n" #'evil-window-next
      :g "C-s-j" #'evil-window-next
      :g "C-s-a" #'evil-window-prev
      :g "C-s-k" #'evil-window-prev)
(map! :g "C-H-s-n" #'evil-window-next
      :g "C-H-s-j" #'evil-window-next
      :g "C-H-s-a" #'evil-window-prev
      :g "C-H-s-k" #'evil-window-prev)
;; Dedicated copy paste key actions
(map! :g "<XF86Copy>" #'evil-yank
      :nvr "<XF86Paste>" #'evil-paste-after
      :i "<XF86Paste>" #'evil-paste-before)
;; Emacs 28 (or native comp?) has some fucked up keycodes??
(map! :g "<269025111>" #'evil-yank
      :nvr "<269025133>" #'evil-paste-after
      :i "<269025133>" #'evil-paste-before)
;; Multi-edit
(map! :g "M-a" #'evil-multiedit-match-all)

(add-hook! 'c-mode-common-hook
  (setq c-tab-always-indent nil)
  (map! :leader
        :desc "Toggle between header and source file" "b o" 'ff-find-other-file))

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
         (:prefix-map ("e" . "dap-evaluate")
          :desc "Evaluate" "e" #'dap-eval
          :desc "Evaluate region" "r" #'dap-eval-region
          :desc "Evaluate at point" "i" #'dap-eval-thing-at-point)
         :desc "Add watch" "a" #'dap-ui-expressions-add-prompt
         :desc "Remove watch" "D" #'dap-ui-expressions-remove
         :desc "Edit template" "t" #'dap-debug-edit-template
         :desc "List breakpoints" "l" #'dap-hydra/dap-ui-breakpoints
         :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle)))

;; Smudge bindings
(after! smudge
  (map! :leader
        (:prefix-map ("j" . "jams")
         :desc "Add to playlist" "a" #'smudge-track-add
         :desc "Create new playlist" "c" #'smudge-create-playlist
         :desc "Select device" "d" #'smudge-select-device
         :desc "Toggle play" "j" #'smudge-controller-toggle-play
         :desc "My playlists" "p" #'smudge-my-playlists
         (:prefix-map ("t" . "smudge-toggle")
          :desc "Toggle shuffle" "s" #'smudge-controller-toggle-shuffle
          :desc "Toggle play" "p" #'smudge-controller-toggle-play
          :desc "Toggle repeat" "r" #'smudge-controller-toggle-repeat)
         (:prefix-map ("s" . "smudge-search")
          :desc "Search for song" "s" #'smudge-track-search
          :desc "Search for playlist" "p" #'smudge-playlist-search))))

;; continuous scrolling
(after! pdf-continuous-scroll-mode
  (map! (:mode pdf-continuous-scroll-mode
          :n "j" #'pdf-continuous-scroll-forward
          :n "<mouse-5>" #'pdf-cs-mouse-scroll-forward
          :n "k" #'pdf-continuous-scroll-backward
          :n "<mouse-4>" #'pdf-cs-mouse-scroll-backward
          :n "J" #'pdf-continuous-next-page
          :n "K" #'pdf-continuous-previous-page
          :n "g t" #'pdf-cscroll-view-goto-page
          :n "g g" #'pdf-cscroll-first-page
          :n "G" #'pdf-cscroll-last-page
          :n "M" #'pdf-cscroll-toggle-mode-line
          :n "q" #'pdf-cscroll-kill-buffer-and-windows
          :n "l" #'pdf-cscroll-image-forward-hscroll
          :n "h" #'pdf-cscroll-image-backward-hscroll)))
