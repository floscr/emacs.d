;; -*- no-byte-compile: t; -*-

;; [[file:~/.config/doom/config.org::*Udiskie][Udiskie:1]]
(package! udiskie :recipe (:host gitlab :repo "floscr/udiskie.el"))
;; Udiskie:1 ends here

;; [[file:~/.config/doom/config.org::*[[https://github.com/tarao/evil-plugins\][Evil Plugin\]] provides some nice addons for Evil Mode][[[https://github.com/tarao/evil-plugins][Evil Plugin]] provides some nice addons for Evil Mode:1]]
(package! evil-plugin :recipe (:host github :repo "tarao/evil-plugins"))
;; [[https://github.com/tarao/evil-plugins][Evil Plugin]] provides some nice addons for Evil Mode:1 ends here

;; [[file:~/.config/doom/config.org::*Evil motions][Evil motions:1]]
(package! evil-replace-with-register)
(package! evil-text-objects-javascript :recipe (:host github :repo "urbint/evil-text-objects-javascript"))
;; Evil motions:1 ends here

;; [[file:~/.config/doom/config.org::*Narrow to an indirect buffer][Narrow to an indirect buffer:1]]
(package! narrow-indirect :recipe (:host github :repo "emacsmirror/narrow-indirect"))
;; Narrow to an indirect buffer:1 ends here

;; [[file:~/.config/doom/config.org::*Colorized hex strings][Colorized hex strings:1]]
(package! rainbow-mode)
;; Colorized hex strings:1 ends here

;; [[file:~/.config/doom/config.org::*Centered buffers, doom does not support this anymore.][Centered buffers, doom does not support this anymore.:1]]
(package! visual-fill-column)
;; Centered buffers, doom does not support this anymore.:1 ends here

;; [[file:~/.config/doom/config.org::*Edit the current chrome input field directly in emacs][Edit the current chrome input field directly in emacs:1]]
(package! atomic-chrome)
;; Edit the current chrome input field directly in emacs:1 ends here

;; [[file:~/.config/doom/config.org::*Filter dired buffers][Filter dired buffers:1]]
(package! dired-narrow)
(package! dired-filter)
;; Filter dired buffers:1 ends here

;; [[file:~/.config/doom/config.org::*Open subtrees directly in the current view][Open subtrees directly in the current view:1]]
(package! dired-subtree)
;; Open subtrees directly in the current view:1 ends here

;; [[file:~/.config/doom/config.org::*Narrow Proced Buffers][Narrow Proced Buffers:1]]
(package! proced-narrow)
;; Narrow Proced Buffers:1 ends here

;; [[file:~/.config/doom/config.org::*Transmission Interface][Transmission Interface:1]]
(package! transmission)
;; Transmission Interface:1 ends here

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

;; [[file:~/.config/doom/config.org::*Literate Calc Mode][Literate Calc Mode:1]]
(package! literate-calc-mode)
;; Literate Calc Mode:1 ends here

;; [[file:~/.config/doom/config.org::*Doom Snippets][Doom Snippets:1]]
(package! doom-snippets :disable t)
(package! my-doom-snippets
  :recipe (:host github
           :repo "floscr/doom-snippets"
           :files ("*.el" "*")))
;; Doom Snippets:1 ends here

;; [[file:~/.config/doom/config.org::*Calfw][Calfw:1]]
(package! calfw :recipe (:host github :repo "floscr/emacs-calfw") :pin "e3d04c253230ed0692f161f527d4e42686060f62")
(package! calfw-org :recipe (:host github :repo "floscr/emacs-calfw") :pin "e3d04c253230ed0692f161f527d4e42686060f62")
(package! calfw-ical :pin "e3d04c253230ed0692f161f527d4e42686060f62")
(package! calfw-cal :disable t)
(package! org-gcal :disable t)
;; Calfw:1 ends here

;; [[file:~/.config/doom/config.org::*Remove those annoying LSP interface plugins][Remove those annoying LSP interface plugins:1]]
(package! lsp-ui :disable t)
(package! merlin-eldoc :disable t)
;; Remove those annoying LSP interface plugins:1 ends here
