;;; ox-scribe.el --- LaTeX Scribe Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Nathanael Gentry, with framework from ox-latex.el

;; Author: Nathanael Gentry <ngentry1@liberty.edu>
;; Keywords: outlines, hypermedia, wp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:
;;
;; See Org manual for details.

;;; Code:

(require 'ox)
(require 'ox-publish)

;; Install a default set-up for scribe export.
(unless (assoc "scribe" org-latex-classes)
    (add-to-list 'org-latex-classes
          '("scribe"
            "\\documentclass{scribe}"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))



;;; User-Configurable Variables

(defgroup org-export-scribe nil
  "Options specific for using the scribe class in LaTeX export."
  :tag "Org Scribe"
  :group 'org-export
  :version "0.71")

(defcustom org-scribe-type "Lecture"
  "Default talk type for a scribe document."
  :group 'org-export-scribe
  :type '(string :tag "Default talk type"))

(defcustom org-scribe-format-drawer-function 'org-scribe-format-drawer
  "Function called to format a drawer in a LaTeX scribe document.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

The default function simply returns the value of CONTENTS."
  :group 'org-export-scribe
  :version "0.81"
  :type 'function)



;;; Define Back-End

(org-export-define-derived-backend 'scribe 'latex
  :translate-alist '((template . org-scribe-template))
  :options-alist '((:latex-class "LATEX_CLASS" nil "scribe" t)
                   (:latex-format-drawer-function nil nil org-scribe-format-drawer-function)
                   (:scribe-course "COURSE" nil nil t)
                   (:scribe-term "TERM" nil nil t)
                   (:scribe-type "TYPE" nil org-scribe-type t)
                   (:scribe-period "PERIOD" nil nil t)
                   (:scribe-lector "LECTOR" nil nil t))
    :menu-entry
    '(?s "Export to Scribe (LaTeX)"
         ((?l "As LaTeX buffer (Scribe)" org-scribe-export-as-latex)
          (?L "As LaTeX file (Scribe)" org-scribe-export-to-latex)
          (?P "As PDF file (Scribe)" org-scribe-export-to-pdf)
          (?o "As PDF file and open (Scribe)"
              (lambda (a s v b)
                (if a (org-scribe-export-to-pdf t s v b)
                  (org-open-file (org-scribe-export-to-pdf nil s v b))))))))



;;;; Transcoders

;;; Drawer

(defun org-scribe-format-drawer (name contents)
  (format "\\begin{%s}\n%s\n\\end{%s}" name contents name))



;;;; Template
;;
;; Template used is simular to the one used in `latex' back-end,
;; with the addition of the extra properties for a scribe document.

(defun org-latex-template (contents info)
  "Return complete document string after LaTeX scribe conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
  (spec (org-latex--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
    (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
         (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; scribe information
     (let ((course (plist-get info :scribe-course))
        (term (plist-get info :scribe-term))
        (type (plist-get info :scribe-type))
        (period (plist-get info :scribe-period))
        (lector (plist-get info :scribe-lector)))
    (format "\\course{%s}\n\\term{%s}\n\\type{%s}\n\\period{%s}\n\\lector{%s}\n"
            course term type period lector))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
      (let ((auth (plist-get info :author)))
        (and auth (org-export-data auth info)))))
     (email (and (plist-get info :with-email)
           (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
        (format "\\author{%s\\thanks{%s}}\n" author email))
       ((or author email) (format "\\author{%s}\n" (or author email)))))
     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
      (formatted-subtitle
       (when subtitle
         (format (plist-get info :latex-subtitle-format)
           (org-export-data subtitle info))))
      (separate (plist-get info :latex-subtitle-separate)))
       (concat
  (format "\\title{%s%s}\n" title
    (if separate "" (or formatted-subtitle "")))
  (when (and separate subtitle)
    (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
  (cond ((not (plist-get info :with-title)) nil)
        ((string= "" title) nil)
        ((not (stringp command)) nil)
        ((string-match "\\(?:[^%]\\|^\\)%s" command)
         (format command title))
        (t command))))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth
   (concat (when (integerp depth)
       (format "\\setcounter{tocdepth}{%d}\n" depth))
     (plist-get info :latex-toc-command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
    (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))


;;; End-user functions
;;;###autoload
(defun org-scribe-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a LaTeX scribe buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Scribe Export (LaTeX)*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'scribe "*Org Scribe Export (LaTeX)*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun org-scribe-convert-region-to-latex ()
  "Assume the current region has Org syntax, and convert it to LaTeX.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in an LaTeX buffer and use this
command to convert it."
  (interactive)
  (org-export-replace-region-by 'scribe))

;;;###autoload
(defun org-scribe-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a LaTeX file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'scribe outfile
      async subtreep visible-only body-only ext-plist)))


;;;###autoload
(defun org-latex-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'scribe outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(provide 'ox-scribe)
(require 'ox-scribe)
;;; ox-scribe.el ends here
