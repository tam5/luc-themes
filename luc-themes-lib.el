;;; luc-themes-lib.el --- Kaolin-themes library, provides common parts for the package  -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO

;;; Code:
(defconst luc-theme-palette
  '(
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

;; Predefined luc face specifications
(defconst luc-theme-faces
  '(
    ;; interface
    (default (:foreground foreground-color :background background-color))
    (cursor (:background cursor-color))
    (hl-line (:inverse-video nil :background highlight-line-color))
    (region (:background selection-color))
    (border-glyph (nil))
    (vertical-border (:background background-color :foreground background-color))
    (linum (:background background-color :foreground subtle-color :slant 'normal :underline nil :distant-foreground subtle-color))
    (fringe (:background background-color))
    (git-gutter-fr:modified (:foreground warning-color :weight 'ultra-light))
    (git-gutter-fr:added (:foreground success-color :weight 'ultra-light))
    (git-gutter-fr:deleted (:foreground error-color :weight 'ultra-light))
    ;; (flycheck-error (:underline (:style wave :color red)))
    ;; (flycheck-warning (:underline (:style wave :color orange)))
    ;; (flymake-warnline (:underline (:style wave :color orange) :background background))
    ;; (flymake-errline (:underline (:style wave :color red) :background background))

    ;; utils
    (bold (:weight 'bold))
    (bold-italic (:slant 'italic :weight 'bold))
    (underline (:underline t))
    (italic (:slant 'italic))

    ;; font lock
    (font-lock-builtin-face (:foreground primitive-color))
    (font-lock-comment-delimiter-face (:foreground comment-color))
    (font-lock-comment-face (:foreground comment-color))
    (font-lock-constant-face (:foreground constant-color))
    (font-lock-doc-face (:foreground comment-color :slant 'italic))
    (font-lock-doc-string-face (:foreground string-color))
    (font-lock-function-name-face (:foreground function-color))
    (font-lock-keyword-face (:foreground keyword-color :slant 'italic))
    (font-lock-negation-char-face (:foreground operator-color))
    (font-lock-regexp-grouping-backslash (:foreground accent-1-color))
    (font-lock-regexp-grouping-construct (:foreground accent-2-color))
    (font-lock-string-face (:foreground string-color))
    (font-lock-type-face (:foreground type-color))
    (font-lock-variable-name-face (:foreground variable-color))
    (font-lock-warning-face (:weight 'bold :foreground warning-color))

    ;; status
    (success (:foreground success-color))
    (error (:foreground error-color))
    (warning (:foreground warning-color))

    ;; mode line
    (mode-line (:foreground foreground-color :background far-background-color))
    (mode-line-buffer-id (:foreground foreground-color :background nil :weight 'bold))
    (mode-line-inactive (:inherit 'mode-line
                                  :foreground subtle-color
                                  :background far-background-color :weight 'normal
                                  :box nil))
    (mode-line-emphasis (:foreground foreground-color :slant 'italic))
    (luc-modeline-position (:foreground comment-color))
    (luc-modeline-git-clean (:foreground success-color))
    (luc-modeline-git-dirty (:foreground warning-color))
    (luc-modeline-git-conflict (:foreground operator-color))
    (luc-modeline-git-ignored (:foreground comment-color))

    ;; sidebar
    (neo-root-dir-face (:foreground supporting-2-color))
    (neo-file-link-face (:foreground supporting-1-color))
    (neo-dir-link-face (:foreground supporting-2-color))
    (neo-dir-icon-face (:foreground supporting-1-color))

    ;; (neo-expand-btn-face (:foreground supporting-2-color))

    ;; dired+
   (diredp-deletion (:foregound error-color :inverse-video t))
   (diredp-deletion-file-name (:foreground error-color))
   (diredp-dir-heading (:foreground keyword-color :weight 'bold))
   (diredp-dir-name (:foreground function-color :background nil))
   (diredp-dir-priv (:foreground primitive-color :background nil))
   (diredp-exec-priv (:foreground warning-color :background nil))
   (diredp-executable-tag (:foreground type-color :background nil))
   (diredp-file-name (:foreground foreground-color))
   (diredp-file-suffix (:foreground foreground-color))
   (diredp-flag-mark (:foreground supporting-2-color :inverse-video t))
   (diredp-flag-mark-line (:background nil :foreground highlight-line-color))
   (diredp-ignored-file-name (:foreground comment-color))
   (diredp-link-priv (:background nil :foreground string-color))
   (diredp-mode-line-flagged (:foreground accent-1-color))
   (diredp-mode-line-marked (:foreground accent-2-color))
   (diredp-no-priv (:background nil))
   (diredp-number (:foreground subtle-color))
   (diredp-symlink (:foreground variable-color))

    ;; Parenthesis matching (built-in)
    (show-paren-match (:underline cursor-color))
    ;; (show-paren-match-face (:background aqua :foreground "black"))

    ;; autocomplete
    (company-preview (:foreground comment-color :background selection-color))
    (company-preview-common (:foreground accent-1-color :background selection-color))
    (company-scrollbar-bg (:background highlight-line-color))
    (company-scrollbar-fg (:background subtle-color))
    (company-tooltip (:weight 'normal :foreground supporting-2-color :background background-color))
    (company-tooltip-common (:weight 'bold :foreground accent-1-color :inherit 'company-tooltip))
    (company-tooltip-selection (:weight 'normal :foreground foreground-color :background selection-color))
    (company-tooltip-common-selection (:weight 'bold :foreground accent-1-color :inherit 'company-tooltip-selection))
    (company-tooltip-annotation (:weight 'normal :foreground function-color))
    (company-tooltip-annotation-selection (:weight 'normal :inherit 'company-tooltip-selection))
    (company-box-candidate (:foreground supporting-2-color))

    ;; (company-echo ((())))
    ;; (company-echo-common ((())))
    ;; ;; (company-preview-search ((())))
    ;; (company-template-field (:background inactive-gray))
    ;; ;; (company-tooltip-mouse ((())))
    ;; ;; (company-tooltip-search ((())))

    ;; Helm
    (helm-header (:foreground foreground-color :background background-color))
    (helm-match (:bold t :foreground accent-1-color))
    (helm-selection (:background highlight-line-color))
    (helm-ff-file (:foreground supporting-2-color))
    (helm-ff-directory (:foreground function-color ))
    (helm-ff-symlink (:foreground type-color ))
    (helm-ff-executable (:foreground accent-2-color ))
    (helm-buffer-directory (:foreground function-color))
    (helm-buffer-file (:foreground supporting-2-color))
    ;; (helm-grep-file (:foreground aqua :underline t))
    ;; (helm-buffer-process (:foreground red))
    ;; (helm-buffer-not-saved (:foreground orange))
    ;; (helm-candidate-number (:foreground foreground :background "#ef6c00"))
    (helm-source-header (:background background-color :foreground foreground-color :height 1.3 :bold t ))

    (link (:foreground nil :underline t))
    (widget-button (:underline t :weight 'bold))
    ;; (widget-field (:background current-line :box (:line-width 1 :color foreground)))

    ;; php-mode
    (php-$this (:foreground accent-1-color :slant 'italic))
    (php-$this-sigil (:foreground operator-color :slant 'italic))
    (php-constant (:foreground constant-color))
    (php-doc-annotation-tag (:foreground keyword-color))
    (php-doc-variable-sigil (:foreground comment-color))
    (php-function-name (:foreground function-color))
    (php-keyword (:foreground keyword-color :slant 'italic))
    (php-method-call (:foreground function-color))
    (php-object-op (:foreground operator-color))
    (php-paamayim-nekudotayim (:foreground operator-color))
    (php-php-tag (:foreground operator-color))
    (php-string (:foreground string-color))
    (php-variable-name (:foreground foreground-color))
    (php-variable-sigil (:foreground operator-color))

    (typescript-this-face (:foreground accent-1-color :slant 'italic))
    (typescript-access-modifier-face (:foreground keyword-color))
    (tide-hl-identifier-face (:background nil :underline cursor-color))

    ;; markdown
    (markdown-header-face-1 (:inherit font-lock-function-name-face :weight 'bold :height 1.3 ))
    (markdown-header-face-2 (:inherit font-lock-function-name-face :weight 'bold :height 1.2 ))
    (markdown-header-face-3 (:inherit font-lock-function-name-face :weight 'bold :height 1.1 ))
    (markdown-header-face-4 (:inherit font-lock-function-name-face :weight 'bold :height 1.1 ))
    (markdown-header-face-5 (:inherit font-lock-function-name-face :weight 'bold :height 1.1 ))
    (markdown-header-face-6 (:inherit font-lock-function-name-face :weight 'bold :height 1.1 ))
    (markdown-header-face-7 (:inherit font-lock-function-name-face :weight 'bold :height 1.1 ))
    (markdown-header-face-8 (:inherit font-lock-function-name-face :weight 'bold :height 1.1 ))
    (markdown-header-face-9 (:inherit font-lock-function-name-face :weight 'bold :height 1.1 ))
    (markdown-header-delimiter-face (:inherit font-lock-function-name-face :weight 'bold
                                              :height 1.2))
    (markdown-url-face (:inherit 'link))
    (markdown-link-face (:foreground function-color :underline t))

    ;; ;; js2-mode
    ;; (js2-warning (:underline orange))
    ;; (js2-error (:foreground nil :underline red))
    ;; (js2-external-variable (:foreground purple))
    ;; (js2-function-param (:foreground blue))
    ;; (js2-instance-member (:foreground blue))
    ;; (js2-private-function-call (:foreground red))

    ;; Extra stuff

    ;; (gui-element (:background current-line :foreground foreground))
    ;; (mode-line-highlight (:foreground purple :box nil))
    ;; (minibuffer-prompt (:foreground blue))
    ;; (secondary-selection (:background secondary-selection))

    ;; (header-line (:inherit 'mode-line :foreground purple :background nil))

    ;; (trailing-whitespace (:foreground red :inverse-video t :underline nil))
    ;; (whitespace-trailing (:foreground red :inverse-video t :underline nil))
    ;; (whitespace-space-after-tab (:foreground red :inverse-video t :underline nil))
    ;; (whitespace-space-before-tab (:foreground red :inverse-video t :underline nil))
    ;; (whitespace-empty (:foreground red :inverse-video t :underline nil))
    ;; (whitespace-line (:background nil :foreground red))
    ;; (whitespace-indentation (:background nil :foreground aqua))
    ;; (whitespace-space (:background nil :foreground selection))
    ;; (whitespace-newline (:background nil :foreground selection))
    ;; (whitespace-tab (:background nil :foreground selection))
    ;; (whitespace-hspace (:background nil :foreground selection))

    ;; Search
    ;; (match (:foreground background :background green :inverse-video nil))
    ;; (isearch (:foreground foreground :background green))
    ;; (isearch-lazy-highlight-face (:foreground background :background green :inverse-video nil))
    ;; (lazy-highlight-face (:foreground background :background green :inverse-video nil))
    ;; (isearch-fail (:background background :inherit font-lock-warning-face :inverse-video t))

    ;; iedit
    ;; (iedit-occurrence (:foreground background :background green))
    ))

(provide 'luc-themes-lib)

;;; luc-themes-lib.el ends here
