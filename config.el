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
(setq doom-font (font-spec :family "Fira Code" :size 14 :height 1.0)
      doom-big-font (font-spec :family "Fira Code" :size 20)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :style "Light" :height 1.0))

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
;; Load git hidden secrets (if present)
(when (file-exists-p! "+secrets")
  (load! "+secrets"))
;; Load org config
(load! "+org-config")
;; Load org prettiness
(load! "+org-style")
;; Load hooks
(load! "+hooks")

;; The consult theme loading that comes with vertico doesn't do always call
;; `doom-reload-theme-hook' so we just use the simple `load-theme' instead
(when (modulep! :completion vertico)
  (after! consult
    (setq custom-safe-themes t)
    (define-key! [remap load-theme] nil)))

(when (modulep! :checkers (spell +aspell))
  (setq ispell-dictionary "canadian"))

;; All the cool kids use tree-sitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Make flycheck slow things down less
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 2))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 1.12))

(use-package! dap-cpptools)

;; continuous scrolling in pdf tools
;; (use-package! pdf-continuous-scroll-mode
;; :hook (pdf-view-mode . pdf-continuous-scroll-mode)
;; :after pdf-tools)

;; Spotify controller
(use-package! smudge)

(use-package! emacs-everywhere)

(setq! evil-ex-substitute-global t)

(after! lsp
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pyright")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyright-remote))
  )

(after! tramp
  (custom-set-variables  '(tramp-remote-path
                           (quote (tramp-own-remote-path)))))

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

(setq +latex-viewers '(pdf-tools))
(add-hook 'latex-mode-hook #'TeX-latex-mode)

;; User customization for Verilog mode
(setq verilog-indent-level             2
      verilog-indent-level-module      2
      verilog-indent-level-declaration 2
      verilog-indent-level-behavioral  2
      verilog-indent-level-directive   2
      verilog-case-indent              2
      verilog-auto-newline             nil)

(when (modulep! :completion (vertico)
                 (after! consult
                   (consult-customize consult-theme
                                      :preview-key
                                      (list nil)))))

(good-scroll-mode 1)
