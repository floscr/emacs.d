;; -*- no-byte-compile: t; -*-
;;; lang/+org/packages.el

(when (featurep! +org-web-tools)
  (package! org-web-tools))

(when (featurep! +org-noter)
  (package! +org-noter))
