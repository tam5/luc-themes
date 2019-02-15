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

;; (defcustom luc-themes-git-gutter-solid nil
;;   "If t, display solid line to highlight git-gutter changes in fringe."
;;   :group 'luc-themes)

(defcustom luc-themes-contrast-mode nil
  "Enable contrast between buffer and rest of ui."
  :group 'luc-themes)

(defvar luc-backgrounds-alist
  '(
    (emacs-lisp-mode . "#1c252a"))

  "Alist matching major modes or buffer names with background colors.
Every cons cell on the alist has the form (CHECK . COLOR) where CHECK
is either a symbol matching the `major-mode' or a regular expression
matching the `buffer-name' of the current buffer. COLOR is a string
representing a valid color, eg. \"red\" or \"f00\".")

(defun luc-set-background-color ()
  "Pick background-color according to `luc-backgrounds-alist'.
The overlay used is stored in `luc-background-overlay'."
  (let ((alist luc-backgrounds-alist)
	background)
    (while (and (not background) alist)
      (if (or (and (stringp (caar alist))
		   (string-match (caar alist) (buffer-name)))
	      (eq major-mode (caar alist)))
	  (setq background (cdar alist))
	(setq alist (cdr alist))))
    ;; cleanup
    (mapc (lambda (o)
	    (when (overlay-get o 'chosig)
	      (delete-overlay o)))
	  (overlays-in (point-min) (point-max)))
    ;; new one
    (when background
      (let ((o (make-overlay (point-min) (point-max)
			     (current-buffer) nil t)))
	(overlay-put o 'face `(:background ,background))
	(overlay-put o 'chosig t)))))

(add-hook 'after-change-major-mode-hook 'luc-set-background-color)


(defun luc-activate-theme (theme &optional no-confirm no-enable)
  "Activate luc theme."
  (interactive
   (list
    (intern (completing-read "Load luc theme: "
			     (mapcar 'symbol-name
				     (luc-available-themes))))
    nil nil))
  (load-theme theme t))

(defun luc-available-themes ()
  "Return a list of luc themes available for loading."
  (let ((sym nil)
        (themes '()))
    (dolist (file (file-expand-wildcards
		   (expand-file-name "themes/*-theme.el") t))
      (setq file (file-name-nondirectory file))
      (and (string-match "\\`\\(.+\\)-theme.el\\'" file)
	   (setq sym (intern (match-string 1 file)))
	   (custom-theme-name-valid-p sym)
	   (push sym themes)))
      (nreverse (delete-dups themes))))

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
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'luc-themes)
;;; luc-themes.el ends here
