;;; kat-mode.el --- sample major mode for editing kat files. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2023, Michalis Kokologiannakis

;; Author: Michalis Kokologiannakis (michalis@mpi-sws.org)
;; Version: 0.1
;; Created: 2022.03.22
;; Keywords: languages

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 3.

;;; Commentary:

;; This mode provides some rudimentary highlighting options for editing kat files.

;; TODO: doc

;;; Code:

(defvar kat-mode-hook nil)

(defvar kat-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-j") 'newline-and-indent)
    map)
  "Keymap for Kat major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kat\\'" . kat-mode))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq kat-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("assume" "let" "assert"))
            (x-types '())
            (x-constants '("include"))
            (x-events '("R" "W" "F" "UR" "UW" "TJ" "TB" "TE" "TC" "NA"
			   "REL" "ACQ" "SC" "RLX"
			   "po-imm" "po" "po-loc-imm" "po-loc" "mo-imm" "mo" "rf" "fr-imm" "fr" "loc-overlap"))
            (x-functions '("save" "coherence" "check" "acyclic" "error" "warning" "unless"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-events-regexp (regexp-opt x-events 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . 'font-lock-type-face)
          (,x-constants-regexp . 'font-lock-constant-face)
          (,x-events-regexp . 'font-lock-builtin-face)
          (,x-functions-regexp . 'font-lock-function-name-face)
          (,x-keywords-regexp . 'font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))


(defun kat-indent-line ()
  "Indent current line as kat code"
  (interactive)
  (beginning-of-line)
  (if (looking-at "^[ \t]*\\(include\\|assume\\|let\\|save\\|assert\\|coherence\\|check\\|acyclic\\|error\\|warning\\|//\\)")
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (progn
        (save-excursion
          (forward-line -1)
	  (if (> (current-indentation) 0)
	      (setq cur-indent (current-indentation))
            (setq cur-indent tab-width))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))

(defvar kat-mode-syntax-table nil "Syntax table for `kat-mode'.")

(setq kat-mode-syntax-table
      (let ((st (make-syntax-table)))
        ;; c++ style comment: “// …”
	(modify-syntax-entry ?\/ ". 12b" st)
        (modify-syntax-entry ?\n "> b" st)
	;; parentheses and brackets
	(modify-syntax-entry ?\( "()" st)
	(modify-syntax-entry ?\[ "(]" st)
	;; - and _ are word constituents
	(modify-syntax-entry ?_ "w" st)
	(modify-syntax-entry ?- "w" st)
        st))


;;;###autoload
(define-derived-mode kat-mode fundamental-mode "kat"
  "Major mode for editing kat files"

  (set-syntax-table kat-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(kat-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'kat-indent-line)
  )

;; add the mode to the `features' list
(provide 'kat-mode)

;;; kat-mode.el ends here
