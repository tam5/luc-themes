;;; luc-facebook-theme.el --- luc version of facebook-theme
;;; Commentary:

;;; Code:
(require 'luc-themes)

(define-luc-theme facebook "Facebook Theme."
  (
    (background-color "#252B39")
    (far-background-color "#252B39")
    (foreground-color "#87BCE6")
    (highlight-line-color "#2F374D")
    (selection-color "#1A1F29")
    (cursor-color "#9EA58B")
    (subtle-color "#747D8E")
    (supporting-1-color "#C3CEE3")
    (supporting-2-color "#D3AFC5")

    (comment-color "#6B7BB5")
    (primitive-color "#18C9C9")
    (constant-color "#18C9C9")
    (function-color "#D3AFC5")
    (keyword-color "#ffffff")
    (operator-color "#ffffff")
    (string-color "#B3B2A2")
    (type-color "#E3C78A")
    (variable-color "#C3CEE3")

    (success-color "SeaGreen2")
    (warning-color "#f78a64")
    (error-color "#ff5370")

    (accent-1-color "#B3B2A2")
    (accent-2-color "#c3e88d"))
  (
   ;; Custom theme set faces
   ;; (default             (:inherit t :weight 'bold))))
    (font-lock-keyword-face (:foreground keyword-color :weight 'bold))
    (php-keyword (:foreground keyword-color :weight 'bold))))

;;; luc-material-theme.el ends here
