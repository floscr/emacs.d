;; -*- lexical-binding: t -*-

(use-package! elfeed
  :commands elfeed
  :config
  (setq elfeed-search-filter "@2-week-ago "
        elfeed-db-directory (concat doom-local-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat doom-local-dir "elfeed/enclosures/")
        elfeed-show-entry-switch #'+rss-popup-pane
        elfeed-show-entry-delete #'+rss/delete-pane
        shr-max-image-proportion 0.6)

  (make-directory elfeed-db-directory t)

  ;; Ensure elfeed buffers are treated as real
  (push (lambda (buf) (string-match-p "^\\*elfeed" (buffer-name buf)))
        doom-real-buffer-functions)

  ;; Enhance readability of a post
  (add-hook 'elfeed-show-mode-hook #'+rss|elfeed-wrap)

  (set-popup-rule! "^\\*elfeed-entry" :size 0.75 :select t :quit nil :ttl t))

(use-package! elfeed-org
  :after (:all org elfeed)
  :config
  (setq rmh-elfeed-org-files (list (+org/expand-org-file-name "Elfeed/Elfeed.org"))))

(map! (:map (elfeed-search-mode-map elfeed-show-mode-map))
      [remap doom/kill-this-buffer] "q"
      [remap kill-this-buffer]      "q"
      [remap kill-buffer]           "q"

      (:map elfeed-search-mode-map
       :n "q"   #'+rss/quit
       :n "r"   #'elfeed-update
       :n "s"   #'elfeed-search-live-filter
       :n "RET" #'elfeed-search-show-entry)

      (:map elfeed-show-mode-map
       :n "q"  #'elfeed-kill-buffer
       :m "j"  #'evil-next-visual-line
       :m "k"  #'evil-previous-visual-line
       :n "gn"      #'+rss/next
       :n "gp"  #'+rss/previous))
