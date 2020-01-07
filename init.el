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

 :tools
 (lookup +devdocs +docsets)
 eval
 editorconfig
 macos
 magit
 rgb
 pdf
 flyspell
 pass
 (flycheck +childframe)
 ;; lsp

 :lang
 nix
 rust
 rest
 data
 emacs-lisp
 purescript
 markdown
 ocaml
 haskell
 (org
  +dragndrop
  +present)
 sh

 :app
 irc
 writeme
 calendar

 :config
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

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; * Config
(setq
 user-mail-address "flo.schroedl@gmail.com"
 user-full-name "Florian Schrödl"
 max-specpdl-size 10000)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/home/floscr/Documents/Org/inbox.org" "/home/floscr/Documents/Org/GTD.org" "/home/floscr/Documents/Org/calendar-family.org" "/home/floscr/Documents/Org/Work/work.org")))
 '(safe-local-variable-values (quote ((+MM-Web-mode 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-dir-face ((t (:foreground "#80899E"))))
 '(diredfl-date-time ((t (:foreground "#49505F"))))
 '(diredfl-dir-name ((t (:foreground "#2DADF2"))))
 '(diredfl-dir-priv ((t (:foreground "#282C34"))))
 '(diredfl-exec-priv ((t (:foreground "#80899E"))))
 '(diredfl-k-modified ((t (:foreground "#FF8E90"))))
 '(diredfl-number ((t (:foreground "#80899E"))))
 '(diredfl-other-priv ((t (:foreground "#80899E"))))
 '(diredfl-read-priv ((t (:foreground "#80899E"))))
 '(diredfl-write-priv ((t (:foreground "#80899E"))))
 '(mu4e-highlight-face ((t (:inherit mu4e-unread-face)))))
