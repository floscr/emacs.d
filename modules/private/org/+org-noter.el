;;; private/org/+org-noter.el -*- lexical-binding: t; -*-

;; When switching pages in PDFs, return the scroll to the top of the page
(after! org-noter
  (map! :map org-noter-notes-mode-map
        :n "[n" #'org-noter-sync-prev-note
        :n "]n" #'org-noter-sync-next-note))

(after! pdf-tools
  :config
  (define-key pdf-view-mode-map (kbd "s-f") 'isearch-forward)
  (map! :map pdf-view-mode-map
        :n "gj" (lambda ()
                  (interactive)
                  (pdf-view-next-page-command)
                  (image-bob))
        :n "gk" (lambda ()
                  (interactive)
                  (pdf-view-previous-page)
                  (image-bob))
        :n "gJ" #'pdf-history-backward
        :n "gK" #'pdf-history-forward
        :n "[n" #'org-noter-sync-prev-note
        :n "]n" #'org-noter-sync-next-note
        :n "s-f" #'pdf-links-isearch-link))
