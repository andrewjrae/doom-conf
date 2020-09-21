;;; +sidefx.el -*- lexical-binding: t; -*-

;; SideFX syntax
(add-hook! 'c-mode-common-hook
  (unsetq-hook! 'after-change-major-mode-hook evil-shift-width)
  (setq tab-width 8)
  (setq c-tab-always-indent nil)
  (setq evil-shift-width 4)
  (map! :leader
        :desc "Toggle between header and source file" "b o" 'ff-find-other-file))

;; Shortcut for project wide diff
(map! :leader
      :desc "View project diff with version control" "p v" 'vc-root-diff)

;; Basic syntax highlighting for VEX
(require 'vfl-mode)
(add-hook! 'vfl-mode-hook
  (unsetq-hook! 'after-change-major-mode-hook evil-shift-width)
  (setq tab-width 8)
  (setq evil-shift-width 4))

(remove-hook! 'dap-ui-mode-hook #'dap-ui-controls-mode)

;; HACK: super ghetto disabling of dap-ui-controls-mode for TUI use
(define-minor-mode dap-ui-controls-mode
  "Displaying DAP visuals."
  :init-value nil
  :global t
  :require 'dap-ui)

(require 'xclip)
(xclip-mode 1)

;; TODO: figure out how to do this properly
;; (dap-register-debug-template
;;   "cpptools::karma"
;;   (list :type "cppdbg"
;;         :request "launch"
;;         :name "cpptools::karma"
;;         :MIMode "gdb"
;;         :args (list "proc/test.usda" "-r" "200" "200")
;;         :program "/home/andrewr/dev/hfs/bin/karma"
;;         :visualizerFile "/home/andrewr/dev/src/admin/nt/sesi.natvis"
;;         :showDisplayString t
;;         :cwd "/home/andrewr/dev_reg/src/tests/karma"))
