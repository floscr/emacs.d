(defvar my-tokenscript-dir "/home/floscr/Code/Work/Hyma/tokenscript-interpreter")
(defvar my-tokenscript-nrepl-port 38573)

(defun my-tokenscript/format-on-save ()
  (let ((default-directory (doom-project-root)))
    (call-process "steam-run" nil nil nil "bunx" "@biomejs/biome" "format" "--write" (buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(defun my-tokenscript/start-nbb-repl ()
  "Start a fresh nbb nREPL server for the tokenscript project. Kills any previous server first and clears the process buffer."
  (interactive)
  (let* ((default-directory (doom-project-root))
         (buffer-name "*nbb-nrepl-server*")
         (process-name "nbb-nrepl-server"))
    (when (get-process process-name)
      (ignore-errors (kill-process process-name)))
    ;; Erase the buffer so we get clean output for the new server
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (erase-buffer)))
    (start-process process-name buffer-name
                   "bun" "run"
                   "--bun" "nbb" "nrepl-server"
                   "--port" (number-to-string my-tokenscript-nrepl-port))
    (message "Started nbb nREPL server on port %d" my-tokenscript-nrepl-port)
    ;; Wait a moment for the server to start, then connect
    (run-with-timer 0.5 nil #'cider-connect-cljs)))

(defun my-tokenscript|start-repl ()
  "Always kill and start a fresh nbb nREPL server and connect to it with CIDER."
  (interactive)
  (ignore-errors (cider-quit))
  (my-tokenscript/start-nbb-repl))

(defun my-tokenscript/on-enter ()
  (apheleia-mode -1)
  (when (eq major-mode #'typescript-mode)
    (setq lsp-diagnostics-provider :flycheck)
    (comment (add-hook 'after-save-hook #'my-tokenscript/format-on-save nil :local)))
  (setq-local cider-connect-default-cljs-params
              (list :host "localhost"
                    :port my-tokenscript-nrepl-port
                    :project-dir (doom-project-root)))
  (setq-local cider-default-cljs-repl 'nbb))

(def-project-mode! my-tokenscript-mode
  :when (s-starts-with? my-tokenscript-dir (doom-project-root))
  :on-enter (my-tokenscript/on-enter))
