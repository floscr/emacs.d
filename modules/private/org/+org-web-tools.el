;;; lang/+org/+org-web-tools.el -*- lexical-binding: t; -*-

(defun +org-web-tools/dwim-at-point ()
  "Pass url to web tools from either:
1. An org link under the cursor
2. An url in the clipboard"
  (interactive)
  (let ((org-url (org-element-property :raw-link (org-element-context)))
        (clipboard-url (current-kill 0)))
    (if org-url
        (message "Reading org url from thing at point")
      (org-web-tools-read-url-as-org org-url)
      (if (string-match url-handler-regexp clipboard-url)
          (message "Reading org url from clipboard")
        (org-web-tools-read-url-as-org clipboard-url)
        (message "No url found")))))

(defun +org-web-tools/backup ()
  "Open the url under the cursor"
  (interactive)
  (let ((url (org-web-tools--read-url))))
  (org-web-tools-read-url-as-org))

(defun +org-web-tools/read-url-at-point ()
  "Open the url under the cursor"
  (interactive)
  (org-web-tools-read-url-as-org (org-web-tools--read-url)))

(defun +org-web-tools|read-url-from-chrome ()
  "Open the url under the cursor"
  (interactive)
  (--> (org-mac-chrome-get-frontmost-url)
       (s-match org-bracket-link-regexp it)
       (nth 1 it)
       org-web-tools-read-url-as-org)
  (visual-line-mode)
  (visual-fill-column-mode)
  (setq display-line-numbers nil))

(use-package! org-web-tools
  :after org
  :commands (+org-web-tools/read-url-at-point))
