;; -*- no-byte-compile: t; -*-

;; [[file:~/.config/doom/config.org::*Evil][Evil:1]]
(package! evil-plugin :recipe (:host github :repo "tarao/evil-plugins"))
;; Evil:1 ends here

;; [[file:~/.config/doom/config.org::*Evil][Evil:2]]
(package! evil-replace-with-register)
(package! evil-text-objects-javascript :recipe (:host github :repo "urbint/evil-text-objects-javascript"))
;; Evil:2 ends here

;; [[file:~/.config/doom/config.org::*UI][UI:1]]
(package! narrow-indirect :recipe (:host github :repo "emacsmirror/narrow-indirect"))
;; UI:1 ends here

;; [[file:~/.config/doom/config.org::*UI][UI:2]]
(package! rainbow-mode)
;; UI:2 ends here

;; [[file:~/.config/doom/config.org::*UI][UI:3]]
(package! visual-fill-column)
;; UI:3 ends here

;; [[file:~/.config/doom/config.org::*External][External:1]]
(package! atomic-chrome)
;; External:1 ends here

;; [[file:~/.config/doom/config.org::*Dired][Dired:1]]
(package! dired-narrow)
(package! dired-filter)
;; Dired:1 ends here

;; [[file:~/.config/doom/config.org::*Dired][Dired:2]]
(package! dired-subtree)
;; Dired:2 ends here

;; [[file:~/.config/doom/config.org::*Dired][Dired:3]]
(package! openwith)
;; Dired:3 ends here

;; [[file:~/.config/doom/config.org::*Elisp][Elisp:1]]
(package! symex)
;; Elisp:1 ends here

;; [[file:~/.config/doom/config.org::*Beancount][Beancount:1]]
(package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))
;; Beancount:1 ends here

;; [[file:~/.config/doom/config.org::*Javascript][Javascript:1]]
(package! indium)
;; Javascript:1 ends here

;; [[file:~/.config/doom/config.org::*Javascript][Javascript:2]]
(package! impatient-mode)
;; Javascript:2 ends here

;; [[file:~/.config/doom/config.org::*Javascript][Javascript:3]]
(package! eslintd-fix)
;; Javascript:3 ends here

;; [[file:~/.config/doom/config.org::*Javascript][Javascript:4]]
(package! js-import :recipe (:host github :repo "floscr/js-import"))
;; Javascript:4 ends here

;; [[file:~/.config/doom/config.org::*Graphql][Graphql:1]]
(package! graphql)
;; Graphql:1 ends here

;; [[file:~/.config/doom/config.org::*Git][Git:1]]
(package! git-lens)
;; Git:1 ends here

;; [[file:~/.config/doom/config.org::*Ebooks][Ebooks:1]]
(package! nov)
;; Ebooks:1 ends here

;; [[file:~/.config/doom/config.org::*Calfw][Calfw:1]]
(package! calfw :recipe (:host github :repo "floscr/emacs-calfw") :pin "e3d04c253230ed0692f161f527d4e42686060f62")
(package! calfw-org :recipe (:host github :repo "floscr/emacs-calfw") :pin "e3d04c253230ed0692f161f527d4e42686060f62")
(package! calfw-ical :pin "e3d04c253230ed0692f161f527d4e42686060f62")
;; Calfw:1 ends here

;; [[file:~/.config/doom/config.org::*Disabled Packages][Disabled Packages:1]]
(package! lsp-ui :disable t)
(package! merlin-eldoc :disable t)
;; Disabled Packages:1 ends here
