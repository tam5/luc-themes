;;; luc-material-theme.el --- luc version of koalin-aurora-theme
;;; Commentary:

;;; Code:
(require 'luc-themes)

(define-luc-theme material "Material Theme."
  (
    (background-color "#263238")
    (far-background-color "#1c252a")
    (foreground-color "#ffeeee")
    (highlight-line-color "#1c252a")
    (selection-color "#31464a")
    (cursor-color "#f3c404")
    (subtle-color "#37454c")
    (supporting-1-color "#597481")
    (supporting-2-color "#afbdc4")

    (comment-color "#546e7a")
    (primitive-color "#aac4ce")
    (constant-color "#ff5370")
    (function-color "#7eaaff")
    (keyword-color "#c792ea")
    (operator-color "#89ddff")
    (string-color "#c3e88d")
    (type-color "#ffcb6b")
    (variable-color "#f78a64")

    (success-color "SeaGreen2")
    (warning-color "#f78a64")
    (error-color "#ff5370")

    (accent-1-color "#ff5370")
    (accent-2-color "#c3e88d")))

;;; luc-material-theme.el ends here
