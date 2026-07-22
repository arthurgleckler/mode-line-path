;;; mode-line-path-tests.el --- Tests for mode-line-path  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Arthur A. Gleckler
;; SPDX-FileCopyrightText: 2026 Arthur A. Gleckler
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for mode-line-path.

;;; Code:

(require 'ert)
(require 'mode-line-path)

;;; mode-line-path--truncate

(ert-deftest mode-line-path-test-truncate-breaks-at-slash ()
  "Truncation should prefer breaking at a \"/\" boundary."
  (let ((mode-line-path-length-limit 20))
    (should (string-match-p "^\\.\\.\\."
			    (mode-line-path--truncate
			     "/home/alice/projects/foo/bar/baz")))
    (should (<= (length (mode-line-path--truncate
			 "/home/alice/projects/foo/bar/baz"))
		20))))

(ert-deftest mode-line-path-test-truncate-with-prefix ()
  "Truncation should preserve PREFIX-LEN leading characters."
  (let ((mode-line-path-length-limit 25))
    (let ((result (mode-line-path--truncate
		   "$ab/one/two/three/four/five/six" 4)))
      (should (string-prefix-p "$ab/" result))
      (should (string-match-p "\\.\\.\\." result))
      (should (<= (length result) 25)))))

(ert-deftest mode-line-path-test-truncate-no-slash-fallback ()
  "Truncation should work even without a \"/\" to break at."
  (let ((mode-line-path-length-limit 10))
    (let ((result (mode-line-path--truncate "abcdefghijklmnopqrst")))
      (should (string-prefix-p "..." result))
      (should (<= (length result) 10)))))

;;; mode-line-path--short-env-vars

(ert-deftest mode-line-path-test-short-env-vars-filters-pwd ()
  "PWD should be excluded."
  (let ((process-environment '("PWD=/home/alice")))
    (should (null (mode-line-path--short-env-vars)))))

(ert-deftest mode-line-path-test-short-env-vars-filters-non-absolute ()
  "Variables with non-absolute values should be excluded."
  (let ((process-environment '("AB=relative/path")))
    (should (null (mode-line-path--short-env-vars)))))

(ert-deftest mode-line-path-test-short-env-vars-filters-home ()
  "Variables whose values expand to \"~/\" should be excluded."
  (let ((process-environment
	 (list (concat "HH=" (expand-file-name "~")))))
    (should (null (mode-line-path--short-env-vars)))))

(ert-deftest mode-line-path-test-short-env-vars-respects-max-length ()
  "Only variables with short enough names should be included."
  (let ((process-environment '("AB=/opt/ab" "LONG=/opt/long"))
	(mode-line-path-variable-name-max-length 3))
    (let ((result (mode-line-path--short-env-vars)))
      (should (assoc "AB" result))
      (should-not (assoc "LONG" result)))))

(ert-deftest mode-line-path-test-short-env-vars-nil-max-length ()
  "When max length is nil, all variable names are accepted."
  (let ((process-environment '("LONGNAME=/opt/long"))
	(mode-line-path-variable-name-max-length nil))
    (should (assoc "LONGNAME" (mode-line-path--short-env-vars)))))

(ert-deftest mode-line-path-test-short-env-vars-predicate ()
  "The predicate should filter variables."
  (let ((process-environment '("AB=/opt/ab" "CD=/opt/cd"))
	(mode-line-path-variable-name-max-length 3)
	(mode-line-path-env-variable-predicate
	 (lambda (name _value) (equal name "CD"))))
    (let ((result (mode-line-path--short-env-vars)))
      (should-not (assoc "AB" result))
      (should (assoc "CD" result)))))

;;; mode-line-path-abbreviate-file-name

(ert-deftest mode-line-path-test-abbreviate-uses-env-var ()
  "Abbreviation should substitute a matching env var prefix."
  (let ((process-environment '("ab=/opt/myproject"))
	(mode-line-path-variable-name-max-length 3)
	(mode-line-path-env-variable-predicate nil)
	(mode-line-path-length-limit 40))
    (mode-line-path-reset)
    (should (equal "$ab/src/main.c"
		   (mode-line-path-abbreviate-file-name
		    "/opt/myproject/src/main.c")))))

(ert-deftest mode-line-path-test-abbreviate-prefers-longest-match ()
  "When multiple env vars match, the longest value should win."
  (let ((process-environment '("ab=/opt" "cd=/opt/myproject"))
	(mode-line-path-variable-name-max-length 3)
	(mode-line-path-env-variable-predicate nil)
	(mode-line-path-length-limit 40))
    (mode-line-path-reset)
    (should (equal "$cd/src/main.c"
		   (mode-line-path-abbreviate-file-name
		    "/opt/myproject/src/main.c")))))

(ert-deftest mode-line-path-test-abbreviate-falls-back ()
  "When no env var matches, fall back to `abbreviate-file-name'."
  (let ((process-environment '())
	(mode-line-path-variable-name-max-length 3)
	(mode-line-path-env-variable-predicate nil)
	(mode-line-path-length-limit 40))
    (mode-line-path-reset)
    (let ((result (mode-line-path-abbreviate-file-name "/tmp/foo")))
      (should (stringp result))
      (should (<= (length result) 40)))))

(ert-deftest mode-line-path-test-abbreviate-truncates-long-result ()
  "Long abbreviated paths should be truncated with ellipsis."
  (let ((process-environment '("ab=/opt"))
	(mode-line-path-variable-name-max-length 3)
	(mode-line-path-env-variable-predicate nil)
	(mode-line-path-length-limit 20))
    (mode-line-path-reset)
    (let ((result (mode-line-path-abbreviate-file-name
		   "/opt/one/two/three/four/five/six/seven")))
      (should (string-prefix-p "$ab/" result))
      (should (string-match-p "\\.\\.\\." result))
      (should (<= (length result) 20)))))

(ert-deftest mode-line-path-test-abbreviate-skips-remote ()
  "Remote (TRAMP) filenames should not be matched against env vars."
  (let ((process-environment '("ab=/home"))
	(mode-line-path-variable-name-max-length 3)
	(mode-line-path-env-variable-predicate nil)
	(mode-line-path-length-limit 80))
    (mode-line-path-reset)
    (let ((result (mode-line-path-abbreviate-file-name
		   "/ssh:host:/home/user/file.el")))
      (should-not (string-prefix-p "$ab/" result)))))

(ert-deftest mode-line-path-test-cache ()
  "Repeated calls should return cached results."
  (let ((process-environment '("ab=/opt/myproject"))
	(mode-line-path-variable-name-max-length 3)
	(mode-line-path-env-variable-predicate nil)
	(mode-line-path-length-limit 40))
    (mode-line-path-reset)
    (let ((first (mode-line-path-abbreviate-file-name
		  "/opt/myproject/src/main.c")))
      (should (equal first
		     (mode-line-path-abbreviate-file-name
		      "/opt/myproject/src/main.c"))))))

;;; mode-line-path-reset

(ert-deftest mode-line-path-test-reset-clears-cache ()
  "Resetting should clear the cache."
  (let ((mode-line-path-length-limit 40))
    (mode-line-path-reset)
    (puthash "/test/path" "cached" mode-line-path--cache)
    (should (equal "cached" (gethash "/test/path" mode-line-path--cache)))
    (mode-line-path-reset)
    (should (null (gethash "/test/path" mode-line-path--cache)))))

(provide 'mode-line-path-tests)

;;; mode-line-path-tests.el ends here.