;;; private/org/config.el -*- lexical-binding: t; -*-

(if (featurep! +org-noter)        (load! "+org-noter"))
(if (featurep! +org-reading-list) (load! "+org-reading-list"))
(if (featurep! +org-web-tools)    (load! "+org-web-tools"))
