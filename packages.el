;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! evil-easymotion)
(package! evil-commentary)
(package! evil-indent-plus)

;; (package! ox-latex)
(package! org-attach-screenshot)
(package! emacs-everywhere)
;; (package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
;;                            :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor"))
;;   :pin "7fa2673c64...")

(when (modulep! :tools lsp)
  (package! lsp-ui)
  ;;(package! company-lsp))
  )

;; tree sitter
;; (package! org-present)

;; tree sitter
(package! tree-sitter)
(package! tree-sitter-langs)

;; suupeerr
(package! org-super-agenda)

;; Org prettiness
(package! mixed-pitch)
(package! org-fragtog)
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
(package! org-pretty-tags)

(package! virtualenvwrapper)

;; (package! pdf-continuous-scroll-mode
;;   :recipe (:host github
;;            :repo "dalanicolai/pdf-continuous-scroll-mode.el"))

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

;; Smudge (spotify controller)
(package! oauth2)
(package! smudge
  :recipe (:host github
           :repo "danielfm/smudge"
           :files ("*.el")))

(package! org
  :recipe (:host github :repo "andrewjrae/org-mode" :branch "fix-engraved-latex")
  :pin "5d356b773b97ae4fc269ed9e552e7f173e3d9871")

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces"))

(package! ox-chameleon
  :recipe (:host github :repo "tecosaur/ox-chameleon"))

(package! good-scroll
  :recipe (:host github :repo "io12/good-scroll.el"))

;; some citation stuff
(package! citar :pin "a6926650114a8091f98bff8c7fd00add82043190")
(package! citeproc :pin "38e70c0a94eeefe86ddefc38dfa8ab2311008774")
(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate") :pin "8f49ccbd337edda01e52da0c75f6a76e2cc976f7")

;; (package! gitconfig-mode
;;           :recipe (:host github :repo "magit/git-modes"
;;                          :files ("gitconfig-mode.el")))
;; (package! gitignore-mode
;;           :recipe (:host github :repo "magit/git-modes"
;;                          :files ("gitignore-mode.el")))
