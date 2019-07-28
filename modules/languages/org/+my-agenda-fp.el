;;; languages/org/+my-agenda-fp.el -*- lexical-binding: t; -*-

(defun +agenda-files-to-buffers (files)
  (-map (lambda (f) (if (file-exists-p f)
                        (org-get-agenda-file-buffer f)
                      (error "No such file %s" f))) files))

(defun +agenda-file-to-entry-list (buffer)
  (with-current-buffer buffer
    (org-with-wide-buffer
     (unless (derived-mode-p 'org-mode) (error "Agenda file %s is not in Org mode" (buffer-file-name buffer)))
     (-->
      (org-element-parse-buffer 'headline)
      (org-element-map it 'headline #'+agenda-tasks-process-headline nil nil 'headline)))))

(defun +agenda-files-to-entry-list (files))

(defun +agenda-tasks-fp ()
  (-> (org-agenda-files nil 'ifmode)
      (+agenda-files-to-buffers)
      (-map +agenda-file-to-entry-list)
      (-flatten))


  (let* (file buffer
         (files (org-agenda-files nil 'ifmode))
         (buffers (+agenda-files-to-buffers files)))))
