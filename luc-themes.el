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
;;
;;; Commentary:
;;
;; TODO
;;
;; -------  This package includes the following themes  -------
;;
;;  * luc-material - 
;;  * luc-material-dark - 
;;  * luc-material-light - 
;;  * luc-aurora - 
;;
;; -------  Configuration example  -------
;;
;;  (require 'luc-themes)
;;  (load-theme 'luc-material)
;;
;;  ;; Apply icons customization for Luc themes, requires the all-the-icons package.
;;  (luc-apply-icons)

;;  ;; Or if you have use-package installed
;;  (use-package luc-themes
;;    :config
;;    (load-theme 'luc-material t)
;;    (luc-apply-icons))
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)
(require 'map)
(require 'color)

(require 'luc-themes-lib)

(defgroup luc-themes nil
  "Luc theme properties."
  :group 'faces)

(defcustom luc-themes-git-gutter-solid nil
  "If t, display solid line to highlight git-gutter changes in fringe."
  :group 'luc-themes)

(defcustom luc-themes-distinct-fringe nil
  "Enable distinct background for fringe and line numbers."
  :group 'luc-themes)

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
  "Return hex value of color in luc-pallete by NAME"
  (car (map-elt luc-palette name)))

(defmacro define-luc-theme (name doc &optional opt-palette opt-faces &rest body)
  "Define new luc theme, using NAME as part of full luc-<name> theme name."
  (let* ((luc-theme-name (luc-themes--make-name name))
         (luc-theme-palette (if opt-palette
                                   (luc-themes--merge-alist luc-palette opt-palette)
                                 luc-palette))
         (luc-theme-faces (if opt-faces
                                   (luc-themes--merge-alist luc-faces opt-faces)
                               luc-faces)))

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

;;;;;;;;;; ###autoload
;; (defun kaolin-treemacs-theme ()
;;   "Enable kaolin-themes treemacs theme with all-the-icons package."
;;   (require 'kaolin-themes-treemacs))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'luc-themes)

;;; luc-themes.el ends here
