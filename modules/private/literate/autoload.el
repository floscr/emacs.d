;;; config/literate/autoload.el -*- lexical-binding: t; -*-

(defvar +literate-config-file
  (concat doom-private-dir "config.org")
  "The file path of your literate config file.")

(defvar +literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file path that `+literate-config-file' will be tangled to, then
byte-compiled from.")

;;;###autoload
(defun +literate-tangle (&optional force-p file)
  "Tangles the current buffer FILE if it has changed."
  (let* ((default-directory doom-private-dir)
         (src-file (expand-file-name (or file buffer-file-name)))
         (dst-file (concat (file-name-sans-extension src-file) ".el")))
    (when (or (file-newer-than-file-p src-file
                                      dst-file)
              force-p)
      (message "Compiling your literate config...")
      (start-process
       "org-tangle" nil "emacs"
       "-q" "--batch"
       "-l" "ob-tangle"
       "--eval" (format "(org-babel-tangle-file %S %S)"
                        src-file dst-file)))))

;;;###autoload
(defalias '+literate/reload #'doom/reload)

;;;###autoload
(defun +literate-recompile-maybe-h ()
  "Recompile config.org if we're editing an org file in our DOOMDIR.

We assume any org file in `doom-private-dir' is connected to your literate
config, and should trigger a recompile if changed."
  (when (and (eq major-mode 'org-mode)
             (file-in-directory-p buffer-file-name doom-private-dir))
    (+literate-tangle 'force)))

;; Recompile our literate config if we modify it
;;;###autoload
(after! org
  (add-hook 'after-save-hook #'+literate-recompile-maybe-h))

;;;###autoload
(defun +literate|tangle ()
  "Tangle the current org buffer."
  (interactive)
  (+literate-tangle t))
