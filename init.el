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

 :emacs
 (dired +icons)
 electric
 vc

 :checkers
 (syntax +childframe)
 spell

 :tools
 (lookup +devdocs +docsets +dictionary)
 eval
 editorconfig
 magit
 rgb
 pdf
 pass
 ;; lsp

 :lang
 nix
 rust
 rest
 data
 emacs-lisp
 markdown
 ocaml
 ;; haskell
 (org
  +dragndrop
  +present)
 sh

 :app
 irc
 calendar

 :config
 literate
 (default +bindings +snippets +evil-commands)

 :private
 reason
 work
 javascript
 (org
  +org-web-tools
  +org-reading-list
  +org-tags))

(provide 'init)
;;; init.el ends here

(setq
 user-mail-address "flo.schroedl@gmail.com"
 user-full-name "Florian Schrödl")
