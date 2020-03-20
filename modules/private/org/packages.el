;; -*- no-byte-compile: t; -*-
;;; lang/+org/packages.el

(when (featurep! +org-web-tools)
  (package! org-web-tools))

(when (featurep! +org-noter)
  (package! +org-noter))

(package! doct :recipe (:host github :repo "progfolio/doct" :branch "master"))

(package! helm-org-rifle)
(package! org-pinboard :recipe (:host github :repo "floscr/org-pinboard"))
(package! org-media-info :recipe (:host github :repo "floscr/org-media-info"))
(package! poporg)
(package! org-ql :recipe (:host github :repo "alphapapa/org-ql"))
(package! org-super-agenda :recipe (:host github :repo "alphapapa/org-super-agenda"))
(package! ob-async)
(package! graphql)
(package! org-download)

(package! org-caldav)
(package! org-noter)
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex" :files ("*.el")))
