;; Project Mode ----------------------------------------------------------------

(defvar my-penpot:dir "/home/floscr/Code/Work/Hyma/penpot/repo")

(defun my-penpot/enter-project ()
  (setq-local cider-connect-default-cljs-params (list :host "localhost" :port 3447 :project-dir my-penpot:dir))
  (setq-local cider-default-cljs-repl 'shadow)
  (setq-local cider-shadow-default-options ":main")
  (setq-local browse-at-remote-preferred-remote-name "penpot"))

(def-project-mode! my-penpot-mode
  :modes '(clojure-mode clojurescript-mode magit-mode)
  :when (s-starts-with? my-penpot:dir (doom-project-root))
  :on-enter (my-penpot/enter-project))
(defun my-penpot/penpot-project? ()
  (s-ends-with? "Code/Work/Hyma/tokens-studio-for-penpot/" (doom-project-root)))

(def-project-mode! my-penpot-project-mode
  :modes '(clojure-mode clojurescript-mode magit-mode)
  :when (my-penpot/penpot-project?)
  :on-enter (progn
              (setq-local my-comment-header:col-char nil)
              (setq-local my-comment-header:col-count 0)
              (let* ((h ";; ===")
                     (comment-modes (a-assoc my-comment-header:mode-comment-start
                                             'my-penpot-project-mode h
                                             'clojurescript-mode h
                                             'clojurec-mode h
                                             'clojure-mode h)))
                (setq-local my-comment-header:mode-comment-start comment-modes))))

(def-project-mode! my-penpot-project-clojurescript-mode
  :modes '(clojurescript-mode my-penpot-project-mode))

;; Clojurescript ---------------------------------------------------------------

(defun my-penpot|cider-eval-buffer ()
  "For some reason `cider-eval-buffer' in penpot does not work. So here's a workaround by selecting the whole buffer and eval it."
  (interactive)
  (save-evil-excursion
   (save-excursion
     (evil-visual-select (point-min) (point-max))
     (let ((inhibit-message t))
       (cider-eval-dwim)))))

(defun my-penpot|test-clojurescript-namespace ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n(t/run-tests *ns*)\n")
    (my-penpot|cider-eval-buffer)
    (evil-undo-pop)
    (buffer "*cider-repl repo/frontend:localhost:3447(cljs:shadow-select)*")))

;; Github ----------------------------------------------------------------------

(defun my-penpot|edit-pr-template ()
  "Create a buffer for PR template."
  (interactive)
  (let ((buf (generate-new-buffer "*pr-template*")))
    (with-current-buffer buf
      (insert
       "### Related Ticket

### Summary

### Steps to reproduce

### Checklist

- [x] Choose the correct target branch; use `develop` by default.
- [x] Provide a brief summary of the changes introduced.
- [x] Add a detailed explanation of how to reproduce the issue and/or verify the fix, if applicable.
- [x] Include screenshots or videos, if applicable.
- [x] Add or modify existing integration tests in case of bugs or new features, if applicable.
- [x] Check CI passes successfully.
- [x] Update the `CHANGES.md` file, referencing the related GitHub issue, if applicable.
"))
    (switch-to-buffer buf)
    (markdown-mode)
    (goto-char (point-min))
    (search-forward "Summary")
    (insert "\n\n")
    (evil-insert-state)))
(defun my-penpot|create-org-issue ()
  (interactive)
  (my-github|create-org-entry "tokens-studio/penpot"))

(defun my-penpot|create-branch-from-issue ()
  (interactive)
  (my-github|make-branch-from-issue "tokens-studio/penpot" :max-title-length 128 :prefix "hyma"))

(defun my-penpot|create-pr ()
  (interactive)
  (let* ((current-branch (magit-get-current-branch))
         (base-branch "develop")
         (owner "penpot")
         (repo "penpot")
         (fork-owner "tokens-studio")
         (pr-url (format "https://github.com/penpot/penpot/compare/%s...%s:%s:%s" base-branch fork-owner repo current-branch)))
    (if (and owner repo current-branch)
        (progn
          (browse-url pr-url)
          (message "Opened PR creation page for branch: %s" current-branch))
      (user-error "Could not determine repository info or current branch"))))

;; Bindings --------------------------------------------------------------------

(map! :map my-penpot-mode-map
      :localleader
      :desc "Create branch from issue" "b" #'my-penpot|create-branch-from-issue
      :desc "Create PR" "p" #'my-penpot|create-pr)

(map!
 :map my-penpot-project-clojurescript-mode-map
 :localleader
 (:prefix ("e" . "eval")
  :desc "buffer" "b" #'my-penpot|cider-eval-buffer)
 (:prefix ("t" . "test")
  :desc "namespace" "n" #'my-penpot|test-clojurescript-namespace))
