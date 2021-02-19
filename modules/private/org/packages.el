;; -*- no-byte-compile: t; -*-
;;; lang/+org/packages.el

;;; Utils

;; Better capture templates
(package! doct :recipe (:host github :repo "progfolio/doct" :branch "master"))
;; dash like util util library for org mode
(package! om :recipe (:host github :repo "ndwarshuis/org-ml"))
;; Browse Org Version of a webpage
(package! org-web-tools)
;; Edit Comments in Org Src Buffers
(package! poporg)
;; Query org items
(package! org-ql)
;; Async execute src blocks
(package! ob-async)
;; Query graphql
(package! graphql)


;;; External Helpers

;; Download from clipboard to attachment
(package! org-download)
;; Sync caldav with org
(package! org-caldav :recipe (:host github :repo "floscr/org-caldav"))
(package! org-journal)
(package! org-alert)


;;; Agenda

;; Agenda groups
(package! org-super-agenda :recipe (:host github :repo "alphapapa/org-super-agenda"))
;; Clocking counsel menu
(package! counsel-org-clock)


;;; Custom Packages

(package! org-pinboard :recipe (:host github :repo "floscr/org-pinboard"))
(package! org-media-info :recipe (:host github :repo "floscr/org-media-info"))


;;; Etc

(package! helm-org-rifle)
(package! org-noter)
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex" :files ("*.el")))
