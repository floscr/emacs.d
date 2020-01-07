;;; private/reason/config.el -*- lexical-binding: t; -*-

(defun rtop ()
  "Launch reason version of utop."
  (interactive)
  (cl-letf ((utop-command "rtop -emacs"))
    (utop)))

(defun +org|org-src-block-refmt-reason-ocaml-toggle ()
  "Convert an Org SRC block from ocaml to reason and vice versa."
  (interactive)
  (save-excursion
    (let* ((old-block (org-element-at-point))
           (old-lang (org-element-property :language old-block))
           (new-lang (if (string= old-lang "ocaml") "reason" "ocaml"))
           (formatter (if (string= old-lang "ocaml") 'refmt-region-ocaml-to-reason 'refmt-region-reason-to-ocaml)))
      (org-edit-special)
      (funcall formatter (point-min) (point-max))
      (org-edit-src-exit)
      (let* ((new-block (org-element-at-point))
             (new-block-parsed (org-element-interpret-data (org-element-put-property (org-element-at-point) :language new-lang)))
             (from (org-element-property :begin new-block))
             (to (org-element-property :end new-block)))
        (delete-region from to)))))

(use-package! flycheck-ocaml
 :after (flycheck merlin)
 :commands
 (flycheck-ocaml-setup)
 :config
 (with-eval-after-load 'merlin
   ;; Enable flycheck checker
   (flycheck-ocaml-setup)))

(use-package merlin
  :after (reason-mode)
  :init
  ;; Disable merlin's own error checking
  ;; We'll use flycheck-ocaml for that
  (setq
   merlin-error-after-save nil
   merlin-command (executable-find "ocamlmerlin"))
  :config
  (add-to-list 'company-backends 'merlin-company-backend))

(use-package flycheck-ocaml
  :after (flycheck merlin)
  :commands
  (flycheck-ocaml-setup)
  :config
  (with-eval-after-load 'reason-mode
    ;; Enable flycheck checker
    (flycheck-ocaml-setup)))

(use-package! reason-mode
  :mode "\\.rei?$"
  :commands (reason-mode)

  :init
  (set-lookup-handlers! 'reason-mode
    :definition #'merlin-locate
    :references #'merlin-occurrences
    :documentation #'merlin-document)
  (setq-hook! reason-mode indent-region-function #'apply-refmt)
  (set-electric! 'some-mode :chars '(?|))
  (set-company-backend! 'reason-mode 'merlin-company-backend)

  (let* ((refmt-bin (executable-find "refmt"))
         (merlin-bin (executable-find "ocamlmerlin"))
         (merlin-base-dir (when merlin-bin
                            (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    (when merlin-bin
      (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
      (setq merlin-command merlin-bin))
    (when refmt-bin
      (setq refmt-command refmt-bin))
    (add-hook! reason-mode
      (merlin-mode)
      (add-hook 'before-save-hook #'refmt-before-save nil t)))

  :config
  (add-hook
   'reason-mode-hook
   (lambda ()
     (merlin-eldoc-disable)
     (setq utop-command "opam config exec -- rtop -emacs")
     (add-hook 'before-save-hook 'refmt-before-save)
     (add-hook 'reason-mode-hook 'merlin-mode)
     (add-hook 'reason-mode-hook 'utop-minor-mode)
     (add-hook 'reason-mode-hook 'flycheck-mode)
     :delight "re")))
