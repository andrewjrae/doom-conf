;;; +bindings.el -*- lexical-binding: t; -*-

;; File for all my general bindings

;; Adds Ctrl-S for saving / exiting vim modes, cause I just can't break that habit ;)
(map! :n "C-s" #'save-buffer
      :ivr "C-s" (lambda () (interactive) (evil-normal-state) (save-buffer)))
(map! :leader
      :desc "Jump to mark" "s m" #'counsel-evil-marks
      :desc "Jump to bookmark" "s M" #'bookmark-jump
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
      :g "C-s-a" #'evil-window-prev)

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

;; Spotify bindings
(after! spotify
  (map! :leader
        (:prefix-map ("j" . "jams")
         :desc "Add to playlist" "a" #'spotify-track-add
         :desc "Create new playlist" "c" #'spotify-create-playlist
         :desc "Select device" "d" #'spotify-select-device
         :desc "Toggle play" "j" #'spotify-toggle-play
         :desc "My playlists" "p" #'spotify-my-playlists
         (:prefix-map ("t" . "spotify-toggle")
          :desc "Toggle shuffle" "s" #'spotify-toggle-shuffle
          :desc "Toggle play" "p" #'spotify-toggle-play
          :desc "Toggle repeat" "r" #'spotify-toggle-repeat)
         (:prefix-map ("s" . "spotify-search")
          :desc "Search for song" "s" #'spotify-track-search
          :desc "Search for playlist" "p" #'spotify-playlist-search)
         :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle)))
