;;; config/literate/init.el -*- lexical-binding: t; -*-

(defvar +literate-config-file
  (concat doom-private-dir "config.org")
  "The file path of your literate config file.")

(defvar +literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file path that `+literate-config-file' will be tangled to, then
byte-compiled from.")

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

;; Let 'er rip!
(+literate-tangle (or doom-reloading-p noninteractive) +literate-config-file)
;; No need to load the resulting file. Doom will do this for us after all
;; modules have finished loading.

;; Recompile our literate config if we modify it
(after! org
  (add-hook 'after-save-hook #'+literate-recompile-maybe-h))


(defun +literate|tangle ()
  "Tangle the current org buffer."
  (interactive)
  (+literate-tangle t))
