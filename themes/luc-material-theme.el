;;; luc-material-theme.el --- luc version of koalin-aurora-theme
;;; Commentary:

;;; Code:
(require 'luc-themes)

(define-luc-theme material "Material Theme."
  (
    (background-color "#232729")
    (far-background-color "#232729")
    (foreground-color "#ffeeee")
    (highlight-line-color "#181d21")
    (selection-color "#31454a")
    (cursor-color "#f3c404")
    (subtle-color "#37454c")
    (supporting-1-color "#78909c")
    (supporting-2-color "#d5e0e5")

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
