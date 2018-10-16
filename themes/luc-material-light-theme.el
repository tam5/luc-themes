;;; luc-material-theme.el --- luc version of koalin-aurora-theme
;;; Commentary:

;;; Code:
(require 'luc-themes)

(define-luc-theme material-light "Material Theme Light."
  (
    (background-color "#fafafa")
    (far-background-color "#fafafa")
    (foreground-color "#80cbc4")
    (highlight-line-color "#edf0f1")
    (selection-color "#dcefed")
    (cursor-color "#85908f")
    (subtle-color "#d0d9dd")
    (supporting-1-color "#90a4ae")
    (supporting-2-color "#96a9b2")

    (comment-color "#ccd7da")
    (primitive-color "#aac4ce")
    (constant-color "#ff5370")
    (function-color "#7eaaff")
    (keyword-color "#6182B8")
    (operator-color "#39ADB5")
    (string-color "#91B859")
    (type-color "#ffcb6b")
    (variable-color "#f78a64")

    (success-color "seagreen2")
    (warning-color "#f78a64")
    (error-color "#ff5370")

    (accent-1-color "#ff5370")
    (accent-2-color "#c3e88d")))

;;; luc-material-theme.el ends here
