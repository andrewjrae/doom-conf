;;; +hooks.el -*- lexical-binding: t; -*-

;; increase latex fragment size for big mode
;; (setq-hook! 'doom-big-font-mode-hook
;;   org-format-latex-options
;;         (plist-put org-format-latex-options :scale 2.0))

;; sync up theme with org stuff
(add-hook! 'doom-load-theme-hook
  (defun +org-refresh-test-colour ()
      (set-face-foreground 'line-number (doom-darken 'violet 0.25))
      (set-face-foreground 'line-number-current-line (doom-darken 'magenta 0.25))))

(add-hook! 'doom-load-theme-hook
  (setq org-preview-latex-image-directory
        (concat doom-cache-dir "org-latex/" (symbol-name doom-theme) "/"))
  (dolist (buffer (doom-buffers-in-mode 'org-mode (buffer-list)))
    (with-current-buffer buffer
      (+org--toggle-inline-images-in-subtree (point-min) (point-max) 'refresh)
      (org-clear-latex-preview (point-min) (point-max))
      (org--latex-preview-region (point-min) (point-max)))))

(add-hook! 'verilog-mode-hook :local #'electric-pair-mode)

(setq-hook! 'pdf-continuous-scroll-mode-hook
  pdf-view-display-size 'fit-width)
;; (add-hook! 'pdf-continuous-scroll-mode-hook :local
;;            #'pdf-cscroll-toggle-mode-line
;;            (setq! pdf-view-display-size 'fit-width))

(add-hook! 'csv-mode-hook
           #'csv-align-mode
           #'toggle-truncate-lines)

(defun ajr/start-org-presentation ()
  (display-line-numbers-mode 0)
  (hide-mode-line-mode 1)
  )

(add-hook! 'org-tree-slide-play-hook
           #'ajr/start-org-presentation)
