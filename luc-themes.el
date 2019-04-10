;;; luc-themes.el --- A set of eye pleasing themes  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Ari Miller

;; Author: Ari Miller <arimiller92@gmail.com>
;; URL: https://github.com/tam5/luc-themes
;; Package-Requires: ((emacs "25.1") (autothemer "0.2.2") (cl-lib "0.6"))
;; Version: 1.4.1
;; Keywords: lucid material dark light theme faces

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; This file is not part of GNU Emacs.

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)
(require 'map)
(require 'color)

(require 'luc-themes-lib)

(defgroup luc-themes nil
  "Luc theme properties."
  :group 'faces)

(defcustom luc-themes-contrast-mode nil
  "Enable contrast between buffer and rest of ui."
  :group 'luc-themes)

(defvar luc-available-themes-list nil
  "List of available themes. See `luc-activate-theme'.")

;;;###autoload
(defun luc-activate-theme (theme &optional no-confirm no-enable)
  "Activate luc theme."
  (interactive
   (list
    (intern (completing-read "Activate luc theme: "
                             (mapcar 'symbol-name luc-available-themes-list)))
    nil nil))
  (load-theme theme t))

(defun luc-themes--make-name (sym)
  "Format luc-<sym> from SYM."
  (intern (format "luc-%s" (symbol-name sym))))

(defun luc-themes--merge-alist (base-alist add-alist)
  "Add elements to BASE-LIST from ADD-LIST without dublicates."
  (let ((res (copy-alist base-alist)))
    (cl-loop for el in add-alist
             do (map-put res (car el) (cdr el)))
    res))

(defun luc-themes-get-hex (name)
  "Return hex value of color in luc-theme-pallete by NAME"
  (car (map-elt luc-theme-palette name)))

(defun luc-themes-apply-editor-bg ()
  "Set the background color of the editor portion of the interface."
  (interactive)
  (let ((bg (face-background 'default))
        (contrast-bg "#1C252A"))
    (dolist (buffer (buffer-list))
      (let ((is-special-buffer (string-match-p "*" (buffer-name buffer))))
        (with-current-buffer buffer
          (if is-special-buffer (face-remap-add-relative 'fringe `(:background "red"))
            (face-remap-add-relative 'default `(:background ,bg))))))))

;; (dolist (buffer (buffer-list))
;;   (unless (string-match-p "*" (buffer-name buffer))
;;       (with-current-buffer buffer
;;         (face-remap-add-relative 'fringe `(:background nil)))))

      ;; (let ((bg (luc-themes-get-hex 'background-color)))

 ;; (set-face-attribute 'fringe nil
                      ;; :foreground (face-foreground 'default)
                      ;; :background (face-background 'default))

(defmacro define-luc-theme (name doc &optional opt-palette opt-faces &rest body)
  "Define new luc theme, using NAME as part of full luc-<name> theme name."
  (let* ((luc-theme-name (luc-themes--make-name name))
         (luc-theme-palette (if opt-palette
                                (luc-themes--merge-alist luc-theme-palette opt-palette)
                              luc-theme-palette))
         (luc-theme-faces (if opt-faces
                              (luc-themes--merge-alist luc-theme-faces opt-faces)
                            luc-theme-faces)))

    `(autothemer-deftheme ,luc-theme-name ,doc

                          ((((class color) (min-colors #xFFFFFF)) ; 24bit gui
                            ((class color) (min-colors #xFF))     ; 256
                            t)                                    ; tty

                           ;; Set palette
                           ,@luc-theme-palette)

                          ;; Set faces
                          ,luc-theme-faces

                          ;; Set vars or execute an arbitrary function body
                          ,@body

                          ;; Provide theme
                          (provide-theme ',luc-theme-name))))

;;;###autoload
(defun luc-add-themes-from-path (path)
  "Add all available themes found in `PATH' to `luc-available-themes-list'.

See `luc-activate-theme', for activating these themes."
  (let ((themes '()))
    (dolist (filepath (file-expand-wildcards
                       (expand-file-name (concat path "*-theme.el")) t))
      (let* ((file (file-name-nondirectory filepath))
             (sym (when (string-match "\\`\\(.+\\)-theme.el\\'" file)
                    (intern (match-string 1 file)))))
        (when (custom-theme-name-valid-p sym)
          (push sym themes))))
    (setq luc-available-themes-list (nreverse (delete-dups themes)))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base))
         (path (or (and (file-directory-p dir) dir)
                   base)))
    (luc-add-themes-from-path path)
    (add-to-list 'custom-theme-load-path path)))

(add-hook 'find-file-hook 'luc-themes-apply-editor-bg)

(provide 'luc-themes)
;;; luc-themes.el ends here
