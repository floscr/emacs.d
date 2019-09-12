;;; Evil Packages
;; TODO Evil-Briefcase does not work
(package! evil-briefcase :recipe (:host github :repo "strickinato/evil-briefcase"))
(package! evil-plugin :recipe (:host github :repo "tarao/evil-plugins"))
(package! evil-replace-with-register)
(package! evil-text-objects-javascript :recipe (:host github :repo "urbint/evil-text-objects-javascript"))

(package! symex)

;;; Javascript Packages
(package! indium)
(package! impatient-mode)

;;; Org-Mode Packages
(package! helm-org-rifle)
(package! org-pinboard :recipe (:host github :repo "floscr/org-pinboard"))
(package! org-media-info :recipe (:host github :repo "floscr/org-media-info"))
(package! poporg)
(package! org-ql :recipe (:host github :repo "alphapapa/org-ql"))
(package! org-super-agenda :recipe (:host github :repo "alphapapa/org-super-agenda"))

;;; Utils
;; Show changes in current branch
(package! git-lens)
;; Image editing utility
(package! blimp)
;; Colorized Hex Strings
(package! rainbow-mode)
; (package! beancount :recipe
;    (:host bitbucket :repo "blais/beancount" :files ("editors/emacs/*.el")))

(package! visual-fill-column)

(package! nov)

(package! org-caldav)

(package! define-word)

;; Install frame cmds
;; (package! frame-fns :recipe (:host wiki))
;; (package! frame-cmds :recipe (:host wiki))

(package! narrow-indirect :recipe (:host emacswiki :files ("narrow-indirect.el")))

(package! dired-recent)
(package! dired-narrow)
(package! dired-subtree)
(package! dired-filter)

;; Disabled packages
(package! lsp-ui :disable t)         ;; Annoying LSP Interface
