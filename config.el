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
;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 14))
(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-big-font (font-spec :family "Fira Code" :size 20)
      doom-variable-pitch-font (font-spec :family "Fira Sans Light"))

;; (custom-set-faces! '(font-lock-comment-face t))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

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

;; Load bindings
(load! "+bindings")
;; Load git hidden secrets
(load! "+secrets")
;; Load org prettiness
(load! "+org-style")
;; Load hooks
(load! "+hooks")

;; (after! solaire-mode
;;   (advice-remove #'org-format-latex #'solaire-mode--org-latex-bg)
;;   (advice-remove #'org-create-formula-image #'solaire-mode--org-latex-bg))

;; Org-mode related patches
;; Makes org a little more vim like (no arrow keys required)
(setq org-attach-screenshot-dirfunction (lambda () (progn "images")))
(after! org
  (use-package! org-attach-screenshot
    :config (setq org-attach-dir-relative t
                  org-attach-screenshot-command-line "escrotum -s %f"))
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-startup-with-latex-preview nil)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  (setq org-highlight-latex-and-related '(native script entities))
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.2))
  ;; better emphasis
  (use-package! org-appear
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-autoemphasis t
          org-appear-autosubmarkers t
          org-appear-autolinks nil)
    ;; for proper first-time setup, `org-appear--set-fragments'
    ;; needs to be run after other hooks have acted.
    (run-at-time nil nil #'org-appear--set-fragments))
  ;; Make inline images a reasonable size in org-mode
  (setq org-image-actual-width 400)
  ;; Set todo keywords
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "TEST(x)"
           "STRT(s)"
           "WAIT(w)"
           "|"
           "DONE(d)"
           "KILL(k)")
          (sequence
           "[ ](T)"
           "[-](S)"
           "[?](W)"
           "|"
           "[X](D)")))
  (add-to-list 'org-todo-keyword-faces
               `("TEST" . (:foreground ,(doom-color 'red) :weight bold))))

(use-package! org-super-agenda
  :commands (org-super-agenda-mode))

;; Add syntax highlighting to latex exports using minted
(after! ox-latex
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (latex . t))))

(when (featurep! :checkers (spell +aspell))
  (setq ispell-dictionary "canadian"))

;; Make flycheck slow things down less
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 2))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 1.12))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package! dap-cpptools)

;; Spotify controller
(use-package! spotify)

(use-package! emacs-everywhere)

(setq! evil-ex-substitute-global t)

(after! evil
  (require 'evil-textobj-anyblock)
  (evil-define-text-object my-evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q" 'my-evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" 'my-evil-textobj-anyblock-a-quote))

;; User customization for Verilog mode
(setq verilog-indent-level             2
      verilog-indent-level-module      2
      verilog-indent-level-declaration 2
      verilog-indent-level-behavioral  2
      verilog-indent-level-directive   2
      verilog-case-indent              2
      verilog-auto-newline             nil)
