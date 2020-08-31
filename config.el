;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Andrew Rae"
      user-mail-address "ajrae.nv@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Org-mode related patches

;; Makes org a little more vim like (no arrow keys required)
(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  ;; Make inline images a reasonable size in org-mode
  (setq org-image-actual-width 400)
;; ece380 report class, mostly to get chapters working
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
            '("ece380-report"
                "\\documentclass[oneside, 12pt]{memoir}"
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;; Adds Ctrl-S for saving / exiting vim modes, cause I just can't break that habit ;)
(map! :n "C-s" #'save-buffer
      :ivr "C-s" (lambda () (interactive) (evil-normal-state) (save-buffer)))
;; Better search and replace
(map! :leader
      :desc "Search and replace" "s r" 'query-replace-regexp)
;; Remapping the old SPC-s-r
(map! :leader
      :desc "Jump to mark" "s m" 'counsel-mark-ring)
(map! :leader
      :desc "Jump to bookmark" "s M" 'bookmark-jump)
;; Better switching between workspaces
(map! :leader
      :desc "Switch to last workspace" "TAB l" #'+workspace/other)
(map! :leader
      :desc "Load workspace from file" "TAB L" #'+workspace/other)

;; Make movement keys work on visual lines
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; Poorly secured ghub tokens
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "/home/ajrae/.emacs.d/.local/etc/.authoinfo.gpg"))

;; LSP-Java support
;; override java command to enable assertions all the time
(when (featurep! :lang java)
  (defcustom dap-java-java-command "java -ea"
    "Path of the java executable."
    :group 'dap-java
    :type 'string))

;; User customization for Verilog mode
(setq verilog-indent-level             2
      verilog-indent-level-module      2
      verilog-indent-level-declaration 2
      verilog-indent-level-behavioral  2
      verilog-indent-level-directive   2
      verilog-case-indent              2
      verilog-auto-newline             nil
      )
