;;; flycheck-golangci-lint.el --- Flycheck checker for golangci-lint -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2024, 2025 Anho Ki
;; Copyright (C) 2018 Wei Jian Gan

;; Author: Wei Jian Gan <weijiangan@outlook.com>
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/flycheck-golangci-lint
;; Keywords: convenience, tools, go
;; Version: 0.3.0
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

(flycheck-def-option-var flycheck-golangci-lint-default nil golangci-lint
  "Default set of linters to enable"
  :type '(choice (const :tag "Not set" nil)
                 (const :tag "standard" standard)
                 (const :tag "all" all)
                 (const :tag "none" none)
                 (const :tag "fast" fast))
  :safe #'symbolp)

(flycheck-def-option-var flycheck-golangci-lint-disable nil golangci-lint
  "Disable specific linter"
  :type '(repeat :tag "Linters" (string :tag "linter"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-enable nil golangci-lint
  "Enable specific linter"
  :type '(repeat :tag "Linters" (string :tag "linter"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-enable-only nil golangci-lint
  "Override linters configuration section to only run the specific linter(s)"
  :type '(repeat :tag "Linters" (string :tag "linter"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-fast-only nil golangci-lint
  "Filter enabled linters to run only fast linters"
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-concurrency nil golangci-lint
  "Number of CPUs to use

Default: Automatically set to match Linux container CPU quota and fall
back to the number of logical CPUs in the machine"
  :type 'number
  :safe #'numberp)

(flycheck-def-option-var flycheck-golangci-lint-modules-download-mode nil golangci-lint
  "Modules download mode

If not empty, passed as -mod=<mode> to go tools"
  :type '(choice (const :tag "Not set" nil)
                 (const :tag "readonly" readonly)
                 (const :tag "vendor" vendor)
                 (const :tag "mod" mod))
  :safe #'symbolp)

(flycheck-def-option-var flycheck-golangci-lint-timeout nil golangci-lint
  "Timeout for total work"
  :type '(choice (const :tag "Not set" nil)
                 string)
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-golangci-lint-tests nil golangci-lint
  "Analyze tests (*_test.go)"
  :type 'boolean
  :safe #'booleanp)

(defun flycheck-golangci-lint-output-parse (output checker buffer)
  "Parse JSON formatted `golangci-lini' errors from OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (let ((msg (car (flycheck-parse-json output)))
        (errors nil))
    (let-alist msg
      (dolist (issue .Issues)
        (let-alist issue
          (push
           (flycheck-error-new-at
            .Pos.Line
            .Pos.Column
            (pcase .Severity
              ((or "error" "high") 'error)
              ((or "info" "low") 'info)
              ((or "warning" "medium" _) 'warning))
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
            "--output.text.path=/dev/null"
            "--output.tab.path=/dev/null"
            "--output.html.path=/dev/null"
            "--output.checkstyle.path=/dev/null"
            "--output.code-climate.path=/dev/null"
            "--output.junit-xml.path=/dev/null"
            "--output.teamcity.path=/dev/null"
            "--output.sarif.path=/dev/null"
            "--output.json.path=stdout"
            (config-file "--config" flycheck-golangci-lint-config)
            (option-flag "--no-config" flycheck-golangci-lint-no-config)
            (option "--default" flycheck-golangci-lint-default)
            (option "--disable" flycheck-golangci-lint-disable
                    list flycheck-option-comma-separated-list)
            (option "--enable" flycheck-golangci-lint-enable
                    list flycheck-option-comma-separated-list)
            (option "--enable-only" flycheck-golangci-lint-enable-only
                    list flycheck-option-comma-separated-list)
            (option-flag "--fast-only" flycheck-golangci-lint-fast-only)
            (option "--concurrency" flycheck-golangci-lint-concurrency)
            (option "--modules-download-mode" flycheck-golangci-lint-modules-download-mode)
            (option "--timeout" flycheck-golangci-lint-timeout)
            (option-flag "--tests" flycheck-golangci-lint-tests))
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
