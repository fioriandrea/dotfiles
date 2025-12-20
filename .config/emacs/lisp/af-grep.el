;;; af-grep.el --- Grep-like functionality in pure elisp -*- lexical-binding: t; -*-

;; Copyright (C) Andrea Fiori

;; Author: Andrea Fiori
;; Package-Version: 20251812.0756
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, extensions, files, matching, tools, unix

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Grep-like functionality implemented in pure Emacs lisp.
;; Useful in environments without grep and find installed (such as MS-Windows).

;;; Code:

(require 'cl-lib)
(require 'xref)

(defun af-grep-read-directory-name (&optional prompt)
  (expand-file-name
   (read-directory-name (or prompt "Base directory: ")
		        default-directory nil t)))

(defun af-grep-read-regexp (&optional prompt)
  (read-regexp (or prompt "Search for")
               'find-tag-default-as-regexp
               'grep-regexp-history))

;;;; Find

(defun af-grep-find-files (directory regexp &optional dir-filter)
  (setq directory (directory-file-name directory))
  (let ((results nil)
        (full-files nil)
        (files (condition-case err
                   (directory-files
                    directory nil
                    directory-files-no-dot-files-regexp)
                 (file-error (message
                              "af-grep-find-files: couldn't get files for dir %S because of %S"
                              directory err)
                             nil))))
    (dolist (file files)
      (let ((full-file (concat directory "/" file)))
        (if (file-directory-p full-file)
            (unless (or (file-symlink-p full-file)
                        (and dir-filter (not (funcall dir-filter directory file))))
              (setq results (nconc results (af-grep-find-files full-file regexp dir-filter))))
          (when (string-match regexp file)
	    (push full-file full-files)))))
    (nconc results (nreverse full-files))))

(defun af-grep-find-files-excluding-vc (directory regexp)
  (af-grep-find-files directory regexp
                      (lambda (parent file)
                        (not (member file vc-directory-exclusion-list)))))

(defmacro af-grep-with-project-files-fallback (&rest body)
  (declare (indent 0))
  (let ((orig (make-symbol "orig")))
    `(cl-letf* ((,orig (symbol-function 'project-files))
                ((symbol-function 'project-files)
                 (lambda (project &optional dirs)
                   (condition-case err
                       (apply ,orig (list project dirs))
                     (error
                      (message "project-files error: %S" err)
                      (mapcan (lambda (d)
                                (af-grep-find-files-excluding-vc d "."))
                              (or dirs (list (project-root project)))))))))
       ,@body)))

;;;###autoload
(defun af-grep-project-find-file (&optional include-all)
  (interactive "P")
  (af-grep-with-project-files-fallback
    (project-find-file include-all)))

;;;###autoload
(defun af-grep-project-find-dir ()
  (interactive)
  (af-grep-with-project-files-fallback
    (project-find-dir)))

;;;; Grep

(defcustom af-grep-insert-files-literally nil
  "Whether af-grep commands should insert files literally or not."
  :type 'boolean
  :group 'af-grep)

(defun af-grep-match-files (files regexp)
  (let ((insert-file-contents-function (if af-grep-insert-files-literally
                                           #'insert-file-contents-literally
                                         #'insert-file-contents)))
    (cl-labels
        ((process-files-and-dirs (files)
           (cl-loop for file in files nconc
                    (condition-case err
                        (if (file-directory-p file)
                            (process-files-and-dirs (af-grep-find-files file "."))
                          (let ((res (process-one-file file)))
                            (when res
                              (list (cons file res)))))
                      (file-error
                       (message "af-grep-match-files: failed to grep %S because of %S"
                                file err)
                       nil))))
         (process-one-file (file)
           (funcall
            insert-file-contents-function file nil nil nil 'if-regular)
           (goto-char (point-min))
           (let ((lines (list (cons -1 "")))
                 (matches nil))
             (while (and (not (eobp))
                         (re-search-forward regexp nil t))
               (let* ((line (line-number-at-pos))
                      (line-beg (line-beginning-position))
                      (match-beg (match-beginning 0))
                      (match-line-start (- match-beg line-beg))
                      (match-len (- (match-end 0) match-beg)))
                 (when (/= (caar lines) line)
                   (push (cons line (buffer-substring-no-properties
                                     line-beg (line-end-position)))
                         lines))
                 (push (cons line (list (cons :match-line-start match-line-start)
                                        (cons :match-len match-len)))
                       matches)
                 (when (= match-len 0)
                   (forward-char 1))))
             (group-matches lines matches)))
         (group-matches (lines matches)
           (let ((grouped (make-hash-table)))
             (cl-loop for (linenr . match) in matches do
                      (push match
                            (gethash linenr grouped '())))
             (cl-loop with matches-by-line = ()
                      for (linenr . linetext) in lines
                      unless (= linenr -1)
                      do (push (list
                                (cons :line linenr)
                                (cons :text linetext)
                                (cons :matches (gethash linenr grouped)))
                               matches-by-line)
                      finally return matches-by-line))))
      (with-temp-buffer
        (process-files-and-dirs files)))))

(defun af-grep-matches-to-xref (matches)
  (cl-loop
   for (file . file-matches) in matches
   nconc (cl-loop
          for file-match in file-matches
          for line = (alist-get :line file-match)
          for text = (alist-get :text file-match)
          for textlen = (length text)
          for line-matches = (alist-get :matches file-match)
          nconc (nreverse
                 (cl-loop
                  with prev-start = textlen
                  with first-match = (car line-matches)
                  with first-start = (alist-get :match-line-start first-match)
                  for line-match in (nreverse line-matches)
                  for start = (alist-get :match-line-start line-match)
                  for len = (alist-get :match-len line-match)
                  for end = (+ start len)
                  for sumstart = (if (= start first-start)
                                     0 start)
                  for summary = (substring text sumstart prev-start)
                  do (add-face-text-property
                      (- start sumstart) (- (+ start len) sumstart)
                      'xref-match t summary)
                  (setq prev-start start)
                  collect (xref-make-match summary
                                           (xref-make-file-location
                                            file line start)
                                           len))))))

(defun af-grep-xrefs-show (regexp files)
  (let ((fetcher (lambda (regexp files)
                   (unless files
                     (user-error "Empty file list"))
                   (let* ((matches (af-grep-match-files files regexp))
                          (xrefs (af-grep-matches-to-xref matches)))
                     (unless xrefs
                       (user-error "No matches for: %s" regexp))
                     xrefs))))
    (xref-show-xrefs (apply-partially fetcher regexp files) nil)))

(defvar af-grep-file-regexp-history nil)

(defun af-grep-read-file-regexp ()
  (read-regexp "File name regexp"
               "." 'af-grep-file-regexp-history))

;;;###autoload
(defun af-grep-rgrep (regexp file-regexp dir)
  (interactive
   (list
    (af-grep-read-regexp)
    (af-grep-read-file-regexp)
    (af-grep-read-directory-name)))
  (af-grep-xrefs-show
   regexp (af-grep-find-files-excluding-vc dir file-regexp)))

;;;###autoload
(defun af-grep-project-find-regexp (regexp)
  (interactive (list (af-grep-read-regexp)))
  (if current-prefix-arg
      (let ((directory (af-grep-read-directory-name))
            (file-regexp (af-grep-read-file-regexp)))
        (af-rgrep regexp file-regexp directory))
    (let* ((pr (project-current t))
           (default-directory (project-root pr))
           (files (af-grep-with-project-files-fallback
                    (project-files pr))))
      (af-grep-xrefs-show regexp files))))

;;;###autoload
(defun af-grep-dired-do-find-regexp (regexp)
  (interactive (list (read-regexp "Find regexp"
                                  nil 'dired-regexp-history))
               dired-mode)
  (af-grep-xrefs-show regexp (dired-get-marked-files)))

;;;###autoload
(defun af-grep-dired-do-occur (regexp &optional nlines)
  (interactive (occur-read-primary-args) dired-mode)
  (multi-occur
   (mapcar #'find-file-noselect (dired-get-marked-files))
   regexp nlines))

(provide 'af-grep)

;;; af-grep.el ends here
