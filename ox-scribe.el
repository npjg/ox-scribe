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

(defcustom org-scribe-environments-extra nil
  "Environments triggered by tags in Org export.
Each entry has 4 elements:

name    Name of the environment
key     Selection key for `org-scribe-select-environment'
open    The opening template for the environment, with the following escapes
        %a   the action/overlay specification
        %A   the default action/overlay specification
        %R   the raw SCRIBE_act value
        %o   the options argument, with square brackets
        %O   the raw SCRIBE_opt value
        %h   the headline text
        %r   the raw headline text (i.e. without any processing)
        %H   if there is headline text, that raw text in {} braces
        %U   if there is headline text, that raw text in [] brackets
close   The closing string of the environment."
  :group 'org-export-scribe
  :version "0.71"
  :type '(repeat
	  (list
	   (string :tag "Environment")
	   (string :tag "Selection key")
	   (string :tag "Begin")
	   (string :tag "End"))))

(defconst org-scribe-environments-default
  '(("verse"          "v" "\\begin{verse}%a %% %h"        "\\end{verse}")
    ("quotation"      "q" "\\begin{quotation}%a %% %h"    "\\end{quotation}")
    ("quote"          "Q" "\\begin{quote}%a %% %h"        "\\end{quote}")
    ("theorem"        "t" "\\begin{theorem}%a[%h]"        "\\end{theorem}")
    ("definition"     "d" "\\begin{definition}%a[%h]"     "\\end{definition}")
    ("example"        "e" "\\begin{example}%a[%h]"        "\\end{example}")
    ("proof"          "p" "\\begin{proof}%a[%h]"          "\\end{proof}"))
  "Environments triggered by properties in Scribe export.
These are the defaults - for user definitions, see
`org-scribe-environments-extra'.")

(defcustom org-scribe-format-headline-function 'org-scribe-format-headline-default-function
  "Function for formatting the headline's text.

This function will be called with six arguments:
TODO      the todo keyword (string or nil)
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string)
TAGS      the tags (list of strings or nil)
INFO      the export options (plist)

The function result will be used in the section format string."
  :group 'org-export-scribe
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)


;;; Internal variables.

(defconst org-scribe-environments-special
  '(("theorem"     "t"))
  "Alist of environments treated in a special way by the back-end.
Keys are environment names, as strings, values are bindings used
in `org-scribe-select-environment'.  Environments listed here,
along with their binding, are hard coded and cannot be modified
through `org-scribe-environments-extra' variable.")

;;; Define Back-End

(org-export-define-derived-backend 'scribe 'latex
  :translate-alist '((template . org-scribe-template))
  :options-alist '((:latex-class "LATEX_CLASS" nil "scribe" t)
                   (:latex-format-headline-function nil nil org-scribe-format-headline-function)
                   (:scribe-environments-extra nil nil org-scribe-environments-extra)
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

;;; Headline

(defun org-scribe-format-headline-default-function
    (todo _todo-type priority text tags _info)
  "Default format function for a headline.
See `org-scribe-format-headline-function' for details."
  (concat
   (and todo (format "{\\bfseries\\sffamily %s} " todo))
   (and priority (format "\\framebox{\\#%c} " priority))
   text
   (and (let ((tags (cl-remove-if (lambda (x) (string-match "^S_" x)) tags)))
          (format "\\hfill{}\\textsc{%s}"
            (mapconcat #'org-latex--protect-text tags ":"))))))



;;;; Template
;;
;; Template used is simular to the one used in `latex' back-end,
;; with the addition of the extra properties for a scribe document.

(defun org-scribe-template (contents info)
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


;;; Minor Mode


(defvar org-scribe-mode-map (make-sparse-keymap)
  "The keymap for `org-scribe-mode'.")
(define-key org-scribe-mode-map "\C-c\C-b" 'org-scribe-select-environment)

;;;###autoload
(define-minor-mode org-scribe-mode
  "Support for editing scribe-oriented Org files."
  nil " Sc" 'org-scribe-mode-map)

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'org-mode
   '((":\\S_[a-z]+\\:" 1 'org-scribe-tag prepend))
   'prepend))

(defface org-scribe-tag '((t (:box (:line-width 1 :color grey40))))
  "The special face for scribe tags."
  :group 'org-export-scribe)

(defun org-scribe-property-changed (property value)
  "Track the SCRIBE_env property with tags.
PROPERTY is the name of the modified property.  VALUE is its new
value."
  (cond
   ((equal property "SCRIBE_env")
    (save-excursion
      (org-back-to-heading t)
      ;; Filter out Scribe-related tags and install environment tag.
      (let ((tags (cl-remove-if (lambda (x) (string-match "^S_" x))
				(org-get-tags nil t)))
	    (env-tag (and (org-string-nw-p value) (concat "S_" value))))
	(org-set-tags (if env-tag (cons env-tag tags) tags))
	(when env-tag (org-toggle-tag env-tag 'on)))))))

(add-hook 'org-property-changed-functions 'org-scribe-property-changed)

(defun org-scribe-allowed-property-values (property)
  "Supply allowed values for PROPERTY."
  (cond
   ((and (equal property "SCRIBE_env")
	 (not (org-entry-get nil (concat property "_ALL") 'inherit)))
    ;; If no allowed values for SCRIBE_env have been defined,
    ;; supply all defined environments
    (mapcar 'car (append org-scribe-environments-special
			 org-scribe-environments-extra
			 org-scribe-environments-default)))))


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
(defun org-scribe-export-to-pdf
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

;;;###autoload
(defun org-scribe-select-environment ()
  "Select the environment to be used by scribe for this entry.
While this uses (for convenience) a tag selection interface, the
result of this command will be that the SCRIBE_env *property* of
the entry is set.

In addition to this, the command will also set a tag as a visual
aid, but the tag does not have any semantic meaning."
  (interactive)
  ;; Make sure `org-scribe-environments-special' has a higher
  ;; priority than `org-scribe-environments-extra'.
  (let* ((envs (append org-scribe-environments-special
		       org-scribe-environments-extra
		       org-scribe-environments-default))
	 (org-current-tag-alist
	  (append '((:startgroup))
		  (mapcar (lambda (e) (cons (concat "S_" (car e))
				       (string-to-char (nth 1 e))))
			  envs)
		  '((:endgroup))))
	 (org-tag-persistent-alist nil)
	 (org-use-fast-tag-selection t)
	 (org-fast-tag-selection-single-key t))
    (org-set-tags-command)
    (let ((tags (org-get-tags nil t)))
      (cond
       ((let* ((tags-re (concat "S_" (regexp-opt (mapcar #'car envs) t)))
	       (env (cl-some (lambda (tag)
			       (and (string-match tags-re tag)
				    (match-string 1 tag)))
			     tags)))
	  (and env (progn (org-entry-put nil "SCRIBE_env" env) t))))
       (t (org-entry-delete nil "SCRIBE_env"))))))


(provide 'ox-scribe)
(require 'ox-scribe)
;;; ox-scribe.el ends here
