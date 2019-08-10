;;; Evil Packages
;; TODO Evil-Briefcase does not work
(package! evil-briefcase :recipe (:fetcher github :repo "strickinato/evil-briefcase"))
(package! evil-plugin :recipe (:fetcher github :repo "tarao/evil-plugins"))
(package! evil-replace-with-register)
(package! evil-text-objects-javascript :recipe (:fetcher github :repo "urbint/evil-text-objects-javascript"))

;;; Javascript Packages
(package! indium)

;;; Org-Mode Packages
(package! helm-org-rifle)
(package! org-pinboard :recipe (:fetcher github :repo "floscr/org-pinboard"))
(package! org-media-info :recipe (:fetcher github :repo "floscr/org-media-info"))
(package! poporg)
(package! org-ql :recipe (:fetcher github :repo "alphapapa/org-ql"))
(package! org-super-agenda :recipe (:fetcher github :repo "alphapapa/org-super-agenda"))

;;; Utils
;; Show changes in current branch
(package! git-lens)
;; Image editing utility
(package! blimp)
;; Colorized Hex Strings
(package! rainbow-mode)
(package! beancount :recipe
   (:fetcher bitbucket :repo "blais/beancount" :files ("editors/emacs/*.el")))

(package! visual-fill-column)

(package! nov)

(package! org-caldav)

(package! define-word)

;; Install frame cmds
(package! frame-fns :recipe (:fetcher wiki))
(package! frame-cmds :recipe (:fetcher wiki))

(package! dired-recent)
(package! dired-narrow)
(package! dired-subtree)

;; Disabled packages
(package! lsp-ui :disable t)         ;; Annoying LSP Interface
(package! treemacs-magit :disable t) ;; Hangs on large projects
