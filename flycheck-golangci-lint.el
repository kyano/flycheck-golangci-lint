;;; flycheck-golangci-lint.el --- Flycheck checker for golangci-lint -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Anho Ki
;; Copyright (C) 2018 Wei Jian Gan

;; Author: Wei Jian Gan <weijiangan@outlook.com>
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/flycheck-golangci-lint
;; Keywords: convenience, tools, go
;; Version: 0.2.0
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
  "Read config from file."
  :type '(choice (const :tag "Not set" nil)
                 (file))
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-golangci-lint-no-config nil golangci-lint
  "Don't read config."
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-golangci-lint-go nil golangci-lint
  "Targeted Go version."
  :type '(choice (const :tag "Not set" nil)
                 string)
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-golangci-lint-timeout nil golangci-lint
  "Timeout for total work (default 1m0s)"
  :type '(choice (const :tag "Not set" nil)
                 string)
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-golangci-lint-fast 'not-set golangci-lint
  "Run only fast linters from enabled linters set (first run won't be fast)"
  :type '(choice (const :tag "Not set" not-set)
                 boolean)
  :safe #'(defun (obj)
              (or (symbolp obj)
                  (booleanp obj))))

(flycheck-def-option-var flycheck-golangci-lint-enable-all 'not-set
                         golangci-lint
  "Enable all linters"
  :type '(choice (const :tag "Not set" not-set)
                 boolean)
  :safe #'(defun (obj)
              (or (symbolp obj)
                  (booleanp obj))))

(flycheck-def-option-var flycheck-golangci-lint-disable-all 'not-set
                         golangci-lint
  "Disable all linters"
  :type '(choice (const :tag "Not set" not-set)
                 boolean)
  :safe #'(defun (obj)
              (or (symbolp obj)
                  (booleanp obj))))

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

(flycheck-def-option-var flycheck-golangci-lint-tests 'not-set golangci-lint
  "Analyze tests (*_test.go) (default true)"
  :type '(choice (const :tag "Not set" not-set)
                 boolean)
  :safe #'(defun (obj)
              (or (symbolp obj)
                  (booleanp obj))))

(flycheck-def-option-var flycheck-golangci-lint-skip-files nil golangci-lint
  "Regexps of files to skip"
  :type '(repeat :tag "Skip" (string :tag "regexp"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-skip-dirs-use-default 'not-set
                         golangci-lint
  "Use or not use default excluded directories (default true)"
  :type '(choice (const :tag "Not set" not-set)
                 boolean)
  :safe #'(defun (obj)
              (or (symbolp obj)
                  (booleanp obj))))

(flycheck-def-option-var flycheck-golangci-lint-skip-dirs nil golangci-lint
  "Regexps of directories to skip"
  :type '(repeat :tag "Skip" (string :tag "regexp"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-golangci-lint-exclude-use-default 'not-set
                         golangci-lint
  "Use or not use default excludes (default true)"
  :type '(choice (const :tag "Not set" not-set)
                 boolean)
  :safe #'(defun (obj)
              (or (symbolp obj)
                  (booleanp obj))))

(flycheck-def-option-var flycheck-golangci-lint-exclude nil golangci-lint
  "Exclude issue by regexp"
  :type '(repeat :tag "Exclude" (string :tag "regexp"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker golangci-lint
  "Fast linters runner for Go

See URL `https://golangci-lint.run/'."
  :command ("golangci-lint" "run"
            "--print-issued-lines=false"
            "--out-format=github-actions"
            (config-file "--configfile=" flycheck-golangci-lint-config)
            (option-flag "--no-config" flycheck-golangci-lint-no-config)
            (option "--go=" flycheck-golangci-lint-go concat)
            (option "--timeout=" flycheck-golangci-lint-timeout concat)
            (eval (unless
                      (string= (symbol-name flycheck-golangci-lint-fast)
                               "not-set")
                    (if flycheck-golangci-lint-fast
                        "--fast=true"
                      "--fast=false")))
            (eval (unless
                      (string= (symbol-name flycheck-golangci-lint-enable-all)
                               "not-set")
                    (if flycheck-golangci-lint-enable-all
                        "--enable-all=true"
                      "--enable-all=false")))
            (eval (unless
                      (string= (symbol-name flycheck-golangci-lint-disable-all)
                               "not-set")
                    (if flycheck-golangci-lint-disable-all
                        "--disable-all=true"
                      "--disable-all=false")))
            (eval (when flycheck-golangci-lint-enable
                    (concat "--enable="
                            (string-join flycheck-golangci-lint-enable
                                         ","))))
            (eval (when flycheck-golangci-lint-disable
                    (concat "--disable="
                            (string-join flycheck-golangci-lint-disable
                                         ","))))
            (eval (when flycheck-golangci-lint-presets
                    (concat "--presets="
                            (mapconcat #'symbol-name
                                       flycheck-golangci-lint-presets
                                       ","))))
            (eval (unless
                      (string= (symbol-name flycheck-golangci-lint-tests)
                               "not-set")
                    (if flycheck-golangci-lint-tests
                        "--tests=true"
                      "--tests=false")))
            (eval (when flycheck-golangci-lint-skip-files
                    (concat "--skip-files="
                            (string-join flycheck-golangci-lint-skip-files
                                         ","))))
            (eval (unless
                      (string= (symbol-name
                                flycheck-golangci-lint-skip-dirs-use-default)
                               "not-set")
                    (if flycheck-golangci-lint-skip-dirs-use-default
                        "--skip-dirs-use-default=true"
                      "--skip-dirs-use-default=false")))
            (eval (when flycheck-golangci-lint-skip-dirs
                    (concat "--skip-dirs="
                            (string-join flycheck-golangci-lint-skip-dirs
                                         ","))))
            (eval (unless
                      (string= (symbol-name
                                flycheck-golangci-lint-exclude-use-default)
                               "not-set")
                    (if flycheck-golangci-lint-exclude-use-default
                        "--exclude-use-default=true"
                      "--exclude-use-default=false")))
            (eval (when flycheck-golangci-lint-exclude
                    (concat "--exclude="
                            (string-join flycheck-golangci-lint-exclude
                                         ","))))
            ".")
  :error-patterns
  ((info line-start
         "::info file=" (file-name)
         ",line=" line ",col=" column "::"
         (message)
         line-end)
   (info line-start
         "::info file=" (file-name)
         ",line=" line "::"
         (message)
         line-end)
   (warning line-start
            "::warning file=" (file-name)
            ",line=" line ",col=" column "::"
            (message)
            line-end)
   (warning line-start
            "::warning file=" (file-name)
            ",line=" line "::"
            (message)
            line-end)
   (error line-start
          "::error file=" (file-name)
          ",line=" line ",col=" column "::"
          (message)
          line-end)
   (error line-start
          "::error file=" (file-name)
          ",line=" line "::"
          (message)
          line-end))
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
