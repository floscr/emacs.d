(defvar my-tokenscript-dir "/home/floscr/Code/Work/Hyma/tokenscript-interpreter")
(defvar my-tokenscript-nrepl-port 38573)

(defun my-tokenscript--in-project-p ()
  "Return t if current buffer is in the tokenscript project."
  (and (doom-project-root)
       (s-starts-with? my-tokenscript-dir (doom-project-root))))

(defun my-tokenscript--in-eca-chat-buffer-p ()
  "Return t if current buffer is an eca-chat buffer in tokenscript project."
  (and (derived-mode-p 'eca-chat-mode)
       (string-match-p "^<eca-chat:" (buffer-name))
       (my-tokenscript--in-project-p)))

(defun my-tokenscript|format-js ()
  (interactive)
  (let ((default-directory (doom-project-root)))
    (call-process "steam-run" nil nil nil "bunx" "@biomejs/biome" "format" "--write" (buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(defun my-tokenscript/start-nbb-repl ()
  (interactive)
  (let* ((default-directory (doom-project-root))
         (buffer-name "*nbb-nrepl-server*")
         (process-name "nbb-nrepl-server"))
    (when (get-process process-name)
      (ignore-errors
        (delete-process process-name)
        ;; Wait for process to actually exit
        (while (eq (process-status process-name) 'run)
          (accept-process-output nil 0.01))))
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (erase-buffer)))
    (let ((proc (start-process process-name buffer-name
                               "bun" "run"
                               "--bun" "nbb" "nrepl-server"
                               "--port" (number-to-string my-tokenscript-nrepl-port))))
      (set-process-filter
       proc
       (lambda (process output)
         (with-current-buffer (process-buffer process)
           ;; Insert output, then color region
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert output)
             (ansi-color-apply-on-region (point-min) (point-max))))))
      (message "Started nbb nREPL server on port %d" my-tokenscript-nrepl-port))
    (run-with-timer 0.5 nil #'cider-connect-cljs)))

(defun my-tokenscript|start-repl ()
  "Always kill and start a fresh nbb nREPL server and connect to it with CIDER."
  (interactive)
  (ignore-errors (cider-quit))
  (my-tokenscript/start-nbb-repl))

(defun my-tokenscript|quick-fix ()
  "Switch to github-copilot/gpt-4.1 model, insert fix message and run it."
  (interactive)
  (let ((message "run `bb fix:all` and fix all errors"))
    (my-eca/select-model "github-copilot/gpt-4.1")
    (eca-chat-send-prompt message)))

(defun my-tokenscript/on-enter ()
  (apheleia-mode -1)
  (when (eq major-mode #'typescript-mode)
    (setq lsp-diagnostics-provider :flycheck)
    (comment (add-hook 'after-save-hook #'my-tokenscript|format-js nil :local)))
  (setq-local cider-connect-default-cljs-params
              (list :host "localhost"
                    :port my-tokenscript-nrepl-port
                    :project-dir (doom-project-root)))
  (setq-local cider-default-cljs-repl 'nbb))

(def-project-mode! my-tokenscript-mode
  :when (or (my-tokenscript--in-project-p)
            (my-tokenscript--in-eca-chat-buffer-p))
  :on-enter (my-tokenscript/on-enter))

;; Also activate the mode in eca-chat buffers when in tokenscript project
(add-hook 'eca-chat-mode-hook
          (lambda ()
            (when (my-tokenscript--in-project-p)
              (my-tokenscript-mode 1))))

(map!
 :map my-tokenscript-mode-map
 :localleader
 :desc "Format" "f" #'my-tokenscript|format-js
 :desc "Quick Fix" "F" #'my-tokenscript|quick-fix)
