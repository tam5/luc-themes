;;; luc-themes-lib.el --- Kaolin-themes library, provides common parts for the package  -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO

;;; Code:
(defconst luc-theme-palette
  '(
    (luc/background-color "#232729")
    (luc/far-background-color "#232729")
    (luc/foreground-color "#ffeeee")
    (luc/highlight-line-color "#181d21")
    (luc/selection-color "#31454a")
    (luc/cursor-color "#f3c404")
    (luc/subtle-color "#37454c")
    (luc/supporting-1-color "#78909c")
    (luc/supporting-2-color "#d5e0e5")

    (luc/comment-color "#546e7a")
    (luc/primitive-color "#aac4ce")
    (luc/constant-color "#ff5370")
    (luc/function-color "#7eaaff")
    (luc/keyword-color "#c792ea")
    (luc/operator-color "#89ddff")
    (luc/string-color "#c3e88d")
    (luc/type-color "#ffcb6b")
    (luc/variable-color "#f78a64")

    (luc/success-color "SeaGreen2")
    (luc/warning-color "#f78a64")
    (luc/error-color "#ff5370")

    (luc/accent-1-color "#ff5370")
    (luc/accent-2-color "#c3e88d")
    ))

;; Predefined luc face specifications
(defconst luc-theme-faces
  '(
    ;; interface
    (default (:foreground luc/foreground-color :background luc/background-color))
    (cursor (:background luc/cursor-color))
    (hl-line (:inverse-video nil :background luc/highlight-line-color))
    (region (:background luc/selection-color))
    (border-glyph (nil))
    (vertical-border (:background luc/background-color :foreground luc/background-color))
    (linum (:background luc/background-color :foreground luc/subtle-color :slant 'normal :underline nil :distant-foreground luc/subtle-color))
    (fringe (:background luc/background-color))
    (git-gutter-fr:modified (:foreground luc/warning-color :weight 'ultra-light))
    (git-gutter-fr:added (:foreground luc/success-color :weight 'ultra-light))
    (git-gutter-fr:deleted (:foreground luc/error-color :weight 'ultra-light))
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
    (font-lock-builtin-face (:foreground luc/primitive-color))
    (font-lock-comment-delimiter-face (:foreground luc/comment-color))
    (font-lock-comment-face (:foreground luc/comment-color))
    (font-lock-constant-face (:foreground luc/constant-color))
    (font-lock-doc-face (:foreground luc/comment-color :slant 'italic))
    (font-lock-doc-string-face (:foreground luc/string-color))
    (font-lock-function-name-face (:foreground luc/function-color))
    (font-lock-keyword-face (:foreground luc/keyword-color :slant 'italic))
    (font-lock-negation-char-face (:foreground luc/operator-color))
    (font-lock-regexp-grouping-backslash (:foreground luc/accent-1-color))
    (font-lock-regexp-grouping-construct (:foreground luc/accent-2-color))
    (font-lock-string-face (:foreground luc/string-color))
    (font-lock-type-face (:foreground luc/type-color))
    (font-lock-variable-name-face (:foreground luc/variable-color))
    (font-lock-warning-face (:weight 'bold :foreground luc/warning-color))

    ;; status
    (success (:foreground luc/success-color))
    (error (:foreground luc/error-color))
    (warning (:foreground luc/warning-color))

    ;; mode line
    (mode-line (:foreground luc/foreground-color :background luc/far-background-color))
    (mode-line-buffer-id (:foreground luc/foreground-color :background nil :weight 'bold))
    (mode-line-inactive (:inherit 'mode-line
                                  :foreground luc/subtle-color
                                  :background luc/far-background-color :weight 'normal
                                  :box nil))
    (mode-line-emphasis (:foreground luc/foreground-color :slant 'italic))
    (luc-modeline-position (:foreground luc/comment-color))
    (luc-modeline-git-clean (:foreground luc/success-color))
    (luc-modeline-git-dirty (:foreground luc/warning-color))
    (luc-modeline-git-conflict (:foreground luc/operator-color))
    (luc-modeline-git-ignored (:foreground luc/comment-color))

    ;; sidebar
    (neo-file-link-face (:foreground luc/supporting-1-color))
    (neo-dir-link-face (:foreground luc/supporting-2-color))
    (neo-root-dir-face (:foreground luc/supporting-2-color))

    ;; Parenthesis matching (built-in)
    (show-paren-match (:underline luc/cursor-color))
    ;; (show-paren-match-face (:background aqua :foreground "black"))

    ;; autocomplete
    (company-scrollbar-bg (:background luc/highlight-line-color))
    (company-scrollbar-fg (:background luc/subtle-color))
    (company-tooltip (:weight 'normal :foreground luc/supporting-2-color :background luc/highlight-line-color))
    (company-tooltip-common (:weight 'bold :foreground luc/accent-1-color :inherit 'company-tooltip))
    (company-tooltip-selection (:weight 'bold :foreground luc/foreground-color :background luc/selection-color))
    (company-tooltip-common-selection (:weight 'bold :foreground luc/accent-1-color :inherit 'company-tooltip-selection))
    (company-tooltip-annotation (:weight 'normal :foreground luc/function-color))
    (company-tooltip-annotation-selection (:weight 'normal :inherit 'company-tooltip-selection))
    
    ;; (company-echo ((())))
    ;; (company-echo-common ((())))
    ;; (company-preview (:foreground comment :background inactive-gray))
    ;; (company-preview-common (:foreground luc/comment-color :background luc/highlight-line-color)) ; same background as highlight-line
    ;; ;; (company-preview-search ((())))
    ;; (company-template-field (:background inactive-gray))
    ;; ;; (company-tooltip-mouse ((())))
    ;; ;; (company-tooltip-search ((())))

    ;; Helm
    (helm-header (:foreground luc/foreground-color :background luc/background-color))
    (helm-match (:bold t :foreground luc/accent-1-color))
    (helm-selection (:background luc/highlight-line-color))
    (helm-ff-file (:foreground luc/supporting-2-color))
    (helm-ff-directory (:foreground luc/function-color ))
    (helm-ff-symlink (:foreground luc/type-color ))
    (helm-ff-executable (:foreground luc/accent-2-color ))
    (helm-buffer-directory (:foreground luc/function-color))
    (helm-buffer-file (:foreground luc/supporting-2-color))
    ;; (helm-grep-file (:foreground aqua :underline t))
    ;; (helm-buffer-process (:foreground red))
    ;; (helm-buffer-not-saved (:foreground orange))
    ;; (helm-candidate-number (:foreground foreground :background "#ef6c00"))
    (helm-source-header (:background luc/background-color :foreground luc/foreground-color :height 1.3 :bold t ))

    (link (:foreground nil :underline t))
    (widget-button (:underline t :weight 'bold))
    ;; (widget-field (:background current-line :box (:line-width 1 :color foreground)))

    ;; php-mode
    (php-$this (:foreground luc/accent-1-color :slant 'italic))
    (php-$this-sigil (:foreground luc/operator-color :slant 'italic))
    (php-constant (:foreground luc/constant-color))
    (php-doc-annotation-tag (:foreground luc/keyword-color))
    (php-doc-variable-sigil (:foreground luc/comment-color))
    (php-function-name (:foreground luc/function-color))
    (php-keyword (:foreground luc/keyword-color :slant 'italic))
    (php-method-call (:foreground luc/function-color))
    (php-object-op (:foreground luc/operator-color))
    (php-paamayim-nekudotayim (:foreground luc/operator-color))
    (php-php-tag (:foreground luc/operator-color))
    (php-string (:foreground luc/string-color))
    (php-variable-name (:foreground luc/foreground-color))
    (php-variable-sigil (:foreground luc/operator-color))

    (typescript-this-face (:foreground luc/accent-1-color :slant 'italic))
    (typescript-access-modifier-face (:foreground luc/keyword-color))

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
    (markdown-link-face (:foreground luc/function-color :underline t))

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
