;;; mode-line-path.el --- Abbreviate file paths in the mode line  -*- lexical-binding: t; -*-

;; Copyright (c) 2013-2026 Arthur A. Gleckler
;; SPDX-FileCopyrightText: 2026 Arthur A. Gleckler
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Arthur A. Gleckler <melpa4aag@speechcode.com>
;; Assisted-by: Claude Code:claude-opus-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/arthurgleckler/mode-line-path
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display an abbreviated file path in the mode line.  Shorten long paths using
;; the values of short environment variable names (three characters or fewer) as
;; prefixes.  If a path is still too long, truncate it with an ellipsis.  The
;; idea is to abbreviate paths enough that it's practical to show them in the
;; mode line.
;;
;; For example, if $e is "/home/alice/.emacs.d":
;;
;;   /home/alice/.emacs.d/lisp/init.el => $e/lisp
;;
;; Use the variable `mode-line-path' in `mode-line-buffer-identification', or
;; directly in `mode-line-format', to display abbreviated paths.  For example:
;;
;;   (setq-default mode-line-buffer-identification
;;		   '("%b" " (" mode-line-path ")"))

;;; Code:

(defgroup mode-line-path nil
  "Abbreviate file paths in the mode line."
  :group 'mode-line
  :prefix "mode-line-path-")

(defvar mode-line-path--cache (make-hash-table :test #'equal)
  "Cache mapping filenames to abbreviated mode-line strings.")

(defvar mode-line-path--cache-time (float-time)
  "Time at which the abbreviation cache was last cleared.")

(defun mode-line-path-reset ()
  "Clear the abbreviation cache."
  (clrhash mode-line-path--cache)
  (setq mode-line-path--cache-time (float-time)))

(defcustom mode-line-path-length-limit 40
  "Maximum length of path display in the mode line."
  :type 'natnum
  :group 'mode-line-path)

(defcustom mode-line-path-variable-name-max-length 3
  "Maximum length of environment variable names used for abbreviation.
If nil, there is no limit."
  :type '(choice (const :tag "No limit" nil)
		 (natnum :tag "Maximum length"))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (mode-line-path-reset))
  :group 'mode-line-path)

(defcustom mode-line-path-env-variable-predicate nil
  "If non-nil, a function to filter environment variables.
Called with two arguments, NAME and VALUE, for each candidate
variable that passes the built-in filters.  Return non-nil to
include the variable."
  :type '(choice (const :tag "No additional filtering" nil)
		 (function :tag "Predicate function"))
  :group 'mode-line-path)

(defun mode-line-path--short-env-vars ()
  "Return an alist mapping short environment variable names to values.
Exclude the variable \"PWD\" and variables whose values expand to \"~/\"
or don't start with \"/\".  Only include variables whose names are no
longer than `mode-line-path-variable-name-max-length' characters, or all
variables if that option is nil.  If
`mode-line-path-env-variable-predicate' is non-nil, exclude variables
for which it returns nil."
  (let ((home (expand-file-name "~/"))
	(max-len mode-line-path-variable-name-max-length)
	(pred mode-line-path-env-variable-predicate))
    (seq-filter
     (lambda (p)
       (and (cdr p)
	    (string-prefix-p "/" (cdr p))
	    (or (null max-len)
		(<= (length (car p)) max-len))
	    (not (equal home (file-name-as-directory (cdr p))))
	    (not (equal (car p) "PWD"))
	    (or (null pred)
		(funcall pred (car p) (cdr p)))))
     (mapcar (lambda (binding)
	       (let ((pos (string-search "=" binding)))
		 (cons (substring binding 0 pos)
		       (and pos (substring binding (1+ pos))))))
	     process-environment))))

(defun mode-line-path--truncate (path &optional prefix-len)
  "Truncate PATH with an ellipsis, breaking at \"/\" when possible.
If PREFIX-LEN is non-nil, preserve that many leading characters and
insert the ellipsis after them.	 Otherwise, insert it at the start."
  (let* ((prefix-len (or prefix-len 0))
	 (budget (- mode-line-path-length-limit prefix-len 3))
	 (tail (substring path prefix-len))
	 (tail-len (length tail))
	 (start (- tail-len budget))
	 (slash (string-search "/" tail start)))
    (concat (substring path 0 prefix-len)
	    "..."
	    (substring tail
		       (if (and slash (< slash (1- tail-len)))
			   slash
			 start)))))

(defun mode-line-path-abbreviate-file-name (filename)
  "Abbreviate FILENAME using short environment variable names.
Fall back to `abbreviate-file-name' with ellipsis if no match."
  (when (> (- (float-time) mode-line-path--cache-time) 1.0)
    (mode-line-path-reset))
  (or (gethash filename mode-line-path--cache)
      (puthash filename
	       (or (and (not (file-remote-p filename))
			(seq-some
			 (lambda (binding)
			   (let ((prefix
				  (concat (expand-file-name (cdr binding))
					  "/")))
			     (when (string-prefix-p prefix filename)
			       (let* ((var-prefix
				       (concat "$" (car binding) "/"))
				      (result
				       (concat var-prefix
					       (substring filename
							  (length prefix)))))
				 (if (> (length result)
					mode-line-path-length-limit)
				     (mode-line-path--truncate
				      result
				      (length var-prefix))
				   result)))))
			 (seq-sort-by (lambda (p) (length (cdr p)))
				      #'>
				      (mode-line-path--short-env-vars))))
		   (let ((abbrev (abbreviate-file-name filename)))
		     (if (> (length abbrev) mode-line-path-length-limit)
			 (mode-line-path--truncate abbrev)
		       abbrev)))
	       mode-line-path--cache)))

;;;###autoload
(defconst mode-line-path
  '(:eval (mode-line-path-abbreviate-file-name
	   (or (and buffer-file-name
		    (file-name-directory buffer-file-name))
	       default-directory)))
  "Mode line construct that displays an abbreviated directory path.
This can be used in `mode-line-format' or
`mode-line-buffer-identification' to show the current directory,
abbreviated using short environment variable names or truncated
with an ellipsis.")
(put 'mode-line-path 'risky-local-variable t)

(provide 'mode-line-path)

;;; mode-line-path.el ends here.