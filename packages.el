;;; Evil Packages
;; TODO Evil-Briefcase does not work
;; (package! evil-briefcase :recipe (:host github :repo "strickinato/evil-briefcase"))
(package! evil-plugin :recipe (:host github :repo "tarao/evil-plugins"))
(package! evil-replace-with-register)
(package! evil-text-objects-javascript :recipe (:host github :repo "urbint/evil-text-objects-javascript"))

(package! symex)

(package! narrow-indirect :recipe (:host github :repo "emacsmirror/narrow-indirect"))

;; (package! beancount :recipe (:host github :repo "beancount/beancount" :files ("editors/emacs/beancount.el")))
(package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))

(package! calfw :recipe (:host github :repo "floscr/emacs-calfw") :pin "e3d04c253230ed0692f161f527d4e42686060f62")
(package! calfw-org :recipe (:host github :repo "floscr/emacs-calfw") :pin "e3d04c253230ed0692f161f527d4e42686060f62")
(package! calfw-ical :pin "e3d04c253230ed0692f161f527d4e42686060f62")

;;; Javascript Packages
(package! indium)
(package! impatient-mode)
(package! eslintd-fix)

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
(package! openwith)

(package! visual-fill-column)

;; Disabled packages
(package! lsp-ui :disable t)         ;; Annoying LSP Interface
(package! merlin-eldoc :disable t)         ;; Annoying LSP Interface
