;;; private/org/+org-reading-list.el -*- lexical-binding: t; -*-

(defun +org-tags/uninherited-tags (scope)
  "List all unique and uninherrited tags in SCOPE."
  (let ((org-use-tag-inheritance nil))
    (--> (org-map-entries #'org-get-tags nil scope)
         -flatten
         -uniq)))

(defun +org-tags/possible-tags ()
  "Get possible tags for the current headline."
  (let* ((file-tags (+org-tags/uninherited-tags 'file))
         (header-tags (org-get-tags nil t)))
    (--reject (-contains? header-tags it) file-tags)))

(defun +org-tags/set-tags (tags)
  "Set tag for item at point."
  (let ((header-tags (--> (org-get-tags nil t)
                          reverse
                          (-snoc it tags))))
    (org-set-tags header-tags)))

(defun +org-tags|set-tags ()
  "Function docstring"
  (interactive)
  (ivy-read "Set tags: " (+org-tags/possible-tags) :action #'+org-tags/set-tags))
