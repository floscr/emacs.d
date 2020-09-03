;;; init.el --- description -*- lexical-binding: t; -*-
(doom!
 :completion
 (company
  +childframe)
 (ivy
  +childframe)

 :ui
 doom
 modeline
 doom-quit
 hl-todo
 (popup +all +defaults)
 vc-gutter
 vi-tilde-fringe
 window-select
 workspaces
 zen

 :email
 (mu4e +gmail)

 :editor
 (evil +everywhere)
 file-templates
 fold
 rotate-text
 multiple-cursors
 parinfer
 snippets

 :term
 eshell
 term
 vterm

 :emacs
 (dired +icons)
 electric
 vc
 (undo +tree)

 :checkers
 (syntax +childframe)
 spell

 :tools
 (lookup
  +devdocs
  +docsets
  +dictionary)
 eval
 editorconfig
 (magit +forge)
 rgb
 pdf
 pass
 docker
 lsp

 :lang
 nix
 rust
 rest
 data
 emacs-lisp
 markdown
 ocaml
 nim
 (javascript +lsp)
 ;; haskell
 (org
  +dragndrop
  +present
  +roam)
 sh

 :app
 irc
 calendar
 (rss +org)

 :config
 (default +bindings +snippets +evil-commands)

 :private
 ;; rss
 system
 literate
 reason
 work
 (org
  +org-web-tools
  +org-reading-list
  +org-tags
  +org-pinboard))

(provide 'init)

(setq
 user-full-name "Florian Schrödl")

;; Select popup buffers by default
(setq +popup-defaults
  (list :side   'bottom
        :height 0.45
        :width  40
        :quit   t
        :select t
        :ttl    5))
