;;; flycheck-golangci-lint.el --- Flycheck checker for golangci-lint -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2024 Anho Ki
;; Copyright (C) 2018 Wei Jian Gan

;; Author: Wei Jian Gan <weijiangan@outlook.com>
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/flycheck-golangci-lint
;; Keywords: convenience, tools, go
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4") (flycheck "32"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flycheck checker for golangci-lint
;;
;; # Setup
;;
;;     (with-eval-after-load 'flycheck-golangci-lint
;;       (add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;;; Code:

(require 'subr-x)
(require 'flycheck)

(flycheck-def-option-var flycheck-golangci-lint-config nil golangci-lint
  "Read config from file path"
  :type '(choice (const :tag "Not set" nil)
                 (file))
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-golangci-lint-no-config nil golangci-lint
  "Don't read config file"
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-go nil golangci-lint
  "Targeted Go version"
  :type '(choice (const :tag "Not set" nil)
                 string)
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-golangci-lint-timeout nil golangci-lint
  "Timeout for total work (default 1m0s)"
  :type '(choice (const :tag "Not set" nil)
                 string)
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-golangci-lint-fast nil golangci-lint
  "Run only fast linters from enabled linters set (first run won't be fast)"
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-enable-all nil golangci-lint
  "Enable all linters"
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-disable-all nil golangci-lint
  "Disable all linters"
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-enable-only nil golangci-lint
  "Override linters configuration section to only run the specific linter(s)"
  :type '(repeat :tag "Linters" (string :tag "linter"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-enable nil golangci-lint
  "Enable specific linter"
  :type '(repeat :tag "Linters" (string :tag "linter"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-disable nil golangci-lint
  "Disable specific linter"
  :type '(repeat :tag "Linters" (string :tag "linter"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-presets nil golangci-lint
  "Enable presets of linters.

Run `golangci-lint linters' to see them.
This option implies option --disable-all"
  :type '(set :tag "Presets"
              (const :tag "bugs" bugs)
              (const :tag "comment" comment)
              (const :tag "complexity" complexity)
              (const :tag "error" error)
              (const :tag "format" format)
              (const :tag "import" import)
              (const :tag "metalinter" metalinter)
              (const :tag "module" module)
              (const :tag "performance" performance)
              (const :tag "sql" sql)
              (const :tag "style" style)
              (const :tag "test" test)
              (const :tag "unused" unused))
  :safe #'flycheck-symbol-list-p)

(flycheck-def-option-var flycheck-golangci-lint-tests nil golangci-lint
  "Analyze tests (*_test.go) (default true)"
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-exclude-files nil golangci-lint
  "Regexps of files to exclude"
  :type '(repeat :tag "Exclude" (string :tag "regexp"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-exclude-dirs-use-default nil golangci-lint
  "Use or not use default excluded directories (default true)

- (^|/)vendor($|/)
- (^|/)third_party($|/)
- (^|/)testdata($|/)
- (^|/)examples($|/)
- (^|/)Godeps($|/)
- (^|/)builtin($|/)"
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-exclude-dirs nil golangci-lint
  "Regexps of directories to exclude"
  :type '(repeat :tag "Exclude" (string :tag "regexp"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-exclude-use-default nil golangci-lint
  "Use or not use default excludes (default true)

See the official help `golangci-lint run --help'"
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-exclude nil golangci-lint
  "Exclude issue by regexp"
  :type '(repeat :tag "Exclude" (string :tag "regexp"))
  :safe #'flycheck-string-list-p)

(defun flycheck-golangci-lint-output-parse (output checker buffer)
  "Parse JSON formatted `golangci-lini' errors from OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (let ((msg (car (flycheck-parse-json output)))
        (errors))
    (let-alist msg
      (dolist (issue .Issues)
        (let-alist issue
          (push
           (flycheck-error-new-at
            .Pos.Line
            .Pos.Column
            (pcase .Severity
              (`"error" 'error)
              (`"high" 'error)
              (`"warning" 'warning)
              (`"medium" 'warning)
              (`"info" 'info)
              (`"low" 'info)
              (_ 'warning))
            .Text
            :checker checker
            :id .FromLinter
            :buffer buffer
            :filename .Pos.Filename)
           errors))))
    (nreverse errors)))

(flycheck-define-checker golangci-lint
  "Fast linters runner for Go

See URL `https://golangci-lint.run/'"
  :command ("golangci-lint" "run"
            "--print-issued-lines=false"
            "--out-format=json"
            (config-file "--config" flycheck-golangci-lint-config)
            (option-flag "--no-config" flycheck-golangci-lint-no-config)
            (option "--go" flycheck-golangci-lint-go)
            (option "--timeout" flycheck-golangci-lint-timeout)
            (option-flag "--fast" flycheck-golangci-lint-fast)
            (option-flag "--enable-all" flycheck-golangci-lint-enable-all)
            (option-flag "--disable-all" flycheck-golangci-lint-disable-all)
            (option "--enable-only" flycheck-golangci-lint-enable-only
                    list flycheck-option-comma-separated-list)
            (option "--enable" flycheck-golangci-lint-enable
                    list flycheck-option-comma-separated-list)
            (option "--disable" flycheck-golangci-lint-disable
                    list flycheck-option-comma-separated-list)
            (option "--presets" flycheck-golangci-lint-presets
                    list flycheck-option-comma-separated-list)
            (option-flag "--tests" flycheck-golangci-lint-tests)
            (option "--exclude-files" flycheck-golangci-lint-exclude-files
                    list flycheck-option-comma-separated-list)
            (option-flag "--exclude-dirs-use-default" flycheck-golangci-lint-exclude-dirs-use-default)
            (option "--exclude-dirs" flycheck-golangci-lint-exclude-dirs
                    list flycheck-option-comma-separated-list)
            (option-flag "--exclude-use-default" flycheck-golangci-lint-exclude-use-default)
            (option "--exclude" flycheck-golangci-lint-exclude
                    list flycheck-option-comma-separated-list))
  :error-parser flycheck-golangci-lint-output-parse
  :modes (go-mode go-ts-mode))

;;;###autoload
(defun flycheck-golangci-lint-setup ()
  "Setup golangci-lint in Flycheck.

Add `golangci-lint' to `flycheck-checkers',
and append it to the checker chains."
  (interactive)
  (add-to-list 'flycheck-checkers 'golangci-lint t)
  (flycheck-add-next-checker 'go-build '(warning . golangci-lint))
  (flycheck-add-next-checker 'go-test '(warning . golangci-lint)))

(provide 'flycheck-golangci-lint)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-golangci-lint.el ends here
