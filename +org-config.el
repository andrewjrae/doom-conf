;;; +org-config.el -*- lexical-binding: t; -*-

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
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  (setq org-highlight-latex-and-related '(native script entities))
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.0))
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

(use-package! org-pandoc-import :after org)

;; add the ignore header option
(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines))))

(after! ox-latex
  (add-to-list 'org-latex-packages-alist '("" "svg"))
  (add-to-list 'org-latex-packages-alist '("" "enumitem"))
  (add-to-list 'org-latex-packages-alist '("" "xcolor"))
  (add-to-list 'org-latex-packages-alist '("" "subcaption"))
  (add-to-list 'org-latex-packages-alist '("hypcap=true" "caption"))
  (setq org-latex-listings 'engraved)
  (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  ;; (setq org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass[headings=standardclasses,parskip=full-]{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-default-class "koma-article")

  (setq org-src-fontify-natively t)

  (add-to-list 'org-src-lang-modes '("python" . python))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (latex . t))))

(use-package! engrave-faces
  :after ox-latex)

(use-package! engrave-faces-latex
  :after ox-latex)

(use-package! ox-chameleon
  :after ox)

;; citations
(use-package! citar
  :when (modulep! :completion ivy))

(use-package! citeproc
  :defer t)

(use-package! oc
  :after org citar
  :config
  (require 'ox)
  (setq org-cite-global-bibliography
        (let ((paths (or citar-bibliography
                         (bound-and-true-p bibtex-completion-bibliography))))
          ;; Always return bibliography paths as list for org-cite.
          (if (stringp paths) (list paths) paths)))
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((t csl))))

  ;;; Org-cite processors
(use-package! oc-biblatex
  :after oc)

(use-package! oc-csl
  :after oc
  :config
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))

(use-package! oc-natbib
  :after oc)
