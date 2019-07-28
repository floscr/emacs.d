(after! dired)

(require 'dired)
(require 'cl)

(defvar my-dired-spawn nil)
(make-variable-buffer-local 'my-dired-spawn)

;; http://stackoverflow.com/a/20023781/2112489
(defun my-dired-kill-last-process-named (name)
"Function initially written by @wvxvw, and revised by @lawlist."
  (let (p)
    (cl-loop with name-re = 
             (format "^%s\\(?:<\\([[:digit:]]+\\)>\\)?" (regexp-quote name))
     for process in (process-list)
     for pname = (process-name process)
     if (string-match name-re pname)
     collect (cons (string-to-number (or (match-string 1 pname) "0")) process)
     into processes
     finally
      (let ((process (cdar (cl-sort processes '> :key 'car))))
        (when (and process (get-process process))
          (delete-process process)
          (setq p process)))) p))

(defun my-dired-qlmanage ()
(interactive)
  (unless (my-dired-kill-last-process-named "qlmanage")
    (let* ((current-node (dired-get-file-for-visit)))
      (set-process-sentinel
        (start-process "qlmanage" nil "/usr/bin/qlmanage" "-p" current-node)
        (lambda (p e)
          (setq e (replace-regexp-in-string "\n$" "" e))
          (cond
            ((and (null my-dired-spawn) (= 9 (process-exit-status p)))
              (message "OFF: my-dired-qlmanage (%s) | %s | %s"
              (process-exit-status p) p e))
            ((and my-dired-spawn (= 9 (process-exit-status p)))
              (message "OFF/ON: my-dired-qlmanage (%s) | %s | %s"
              (process-exit-status p) p e)
              (my-dired-kill-spawn))
            ((= 0 (process-exit-status p))
              (message "OFF (mouse clicked): my-dired-qlmanage (%s) | %s | %s"
              (process-exit-status p) p e))
            (t
              (message "ABNORMAL: my-dired-qlmanage (%s) | %s | %s"
              (process-exit-status p) p e))))))))

(defun my-dired-kill-spawn ()
"This is essentially a three level incursion, starting with `my-dired-qlmanage'
and then calling `my-dired-kill-spawn' twice."
(interactive)
  (let* ((current-node (dired-get-file-for-visit)))
    (set-process-sentinel
      (start-process "qlmanage" nil "/usr/bin/qlmanage" "-p" current-node)
      (lambda (p e)
        (setq e (replace-regexp-in-string "\n$" "" e))
        (cond
          ((and (null my-dired-spawn) (= 9 (process-exit-status p)))
            (message "OFF: my-dired-kill-spawn (%s) | %s | %s"
              (process-exit-status p) p e))
          ((and my-dired-spawn (= 9 (process-exit-status p)))
            (message "OFF/ON: my-dired-kill-spawn (%s) | %s | %s"
              (process-exit-status p) p e)
            (my-dired-kill-spawn))
          ((= 0 (process-exit-status p))
            (message "OFF (mouse clicked): my-dired-kill-spawn (%s) | %s | %s"
              (process-exit-status p) p e))
          (t
            (message "ABNORMAL: my-dired-kill-spawn (%s) | %s | %s"
              (process-exit-status p) p e)))))))

(defun my-dired-previous-line (arg)
(interactive "^p")
  (dired-previous-line arg)
  (let ((my-dired-spawn t))
    (my-dired-kill-last-process-named "qlmanage")))

(defun my-dired-next-line (arg)
(interactive "^p")
  (dired-next-line arg)
  (let ((my-dired-spawn t))
    (my-dired-kill-last-process-named "qlmanage")))

(defun my-dired-quicklook ()
(interactive)
  (my-dired-qlmanage))

(eval-after-load "dired" '(progn
  (define-key dired-mode-map [down] 'my-dired-next-line)
  (define-key dired-mode-map [up] 'my-dired-previous-line)
  (define-key dired-mode-map (kbd "SPC") 'my-dired-quicklook)))
