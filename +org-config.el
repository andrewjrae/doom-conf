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

(use-package! org-pandoc-import :after org)

;; add the ignore header option
(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines))))

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

;; (use-package! org-fragtog
;;   :hook (org-mode  org-fragtog-mode))

;; org present
;; (defun ajr/org-present-prepare-slide ()
;;   (org-overview)
;;   (org-show-entry)
;;   (org-show-children))

;; (defun ajr/org-present-hook ()
;;   ;; (doom-big-font-mode 1)
;;   (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
;;                                      (header-line (:height 4.5) variable-pitch)
;;                                      (org-code (:height 1.25) org-code)
;;                                      (org-verbatim (:height 1.25) org-verbatim)
;;                                      (org-block (:height 1.25) org-block)
;;                                      (org-block-begin-line (:height 0.7) org-block)))
;;   (setq header-line-format " ")
;;   (org-display-inline-images)
;;   (ajr/org-present-prepare-slide))

;; (defun ajr/org-present-quit-hook ()
;;   ;; (doom-big-font-mode 0)
;;   (setq-local face-remapping-alist '((default variable-pitch default)))
;;   (setq header-line-format nil)
;;   (org-present-small)
;;   (org-remove-inline-images))

;; (defun ajr/org-present-prev ()
;;   (interactive)
;;   (org-present-prev)
;;   (ajr/org-present-prepare-slide))

;; (defun ajr/org-present-next ()
;;   (interactive)
;;   (org-present-next)
;;   (ajr/org-present-prepare-slide))

;; (use-package! org-present
;;   :hook ((org-present-mode . ajr/org-present-hook)
;;          (org-present-mode-quit . ajr/org-present-quit-hook)))
;; (map! :map org-present-mode-keymap
;; ;; (map! :mode org-present-mode
;;       :n "s" #'ajr/org-present-next
;;       :n "t" #'ajr/org-present-prev
;;       :g "C-SPC" #'ajr/org-present-next
;;       :g "C-e" #'ajr/org-present-prev)
;;       ;; :n "s" #'ajr/org-present-next
;;       ;; :n "t" #'ajr/org-present-prev
;;       ;; :g "C-SPC" #'ajr/org-present-next
;;       ;; :g "C-e" #'ajr/org-present-prev)
