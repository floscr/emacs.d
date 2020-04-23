;;; Evil Packages
;; TODO Evil-Briefcase does not work
;; (package! evil-briefcase :recipe (:host github :repo "strickinato/evil-briefcase"))
(package! evil-plugin :recipe (:host github :repo "tarao/evil-plugins"))
(package! evil-replace-with-register)
(package! evil-text-objects-javascript :recipe (:host github :repo "urbint/evil-text-objects-javascript"))

(package! symex)

;;; Javascript Packages
(package! indium)
(package! impatient-mode)

;;; Org-Mode Packages

;;; Utils
;; Show changes in current branch
(package! git-lens)
;; Colorized Hex Strings
(package! rainbow-mode)

(package! visual-fill-column)

(package! nov)


(package! define-word)

(package! atomic-chrome)

(package! dired-narrow)
(package! dired-subtree)
(package! dired-filter)

(package! visual-fill-column)

;; Disabled packages
(package! lsp-ui :disable t)         ;; Annoying LSP Interface
(package! merlin-eldoc :disable t)         ;; Annoying LSP Interface
