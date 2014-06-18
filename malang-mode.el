;; Copyright (C) 2014 Mattias Ugelvik
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



(defun empty-linep ()
  "Is the current line empty?"
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*$")))

(defun end-of-linep ()
  (looking-at "$"))

(defun previous-content-line ()
  "Go to the previous line which is not empty.
Returns t if it found a line, nil otherwise"
  (cond ((= (forward-line -1) -1) nil)
        ((empty-linep) (previous-content-line))
        (t t)))

(defun find-indentation ()
  "Find indentation for line at point, searching backwards if line is empty"
  (save-excursion
    (when (empty-linep)
      (previous-content-line))
    (beginning-of-line)
    (skip-chars-forward " \t")))


(defun line-begins (regex)
  "Does the current line begin with REGEX?"
  (save-excursion
    (beginning-of-line)
    (let ((case-fold-search nil)) (looking-at regex))))

(defun line-ends (regex)
  "Does the current line end with REGEX?"
  (save-excursion
    (end-of-line)
    (let ((case-fold-search nil)) (looking-back regex))))


(defun previous-begins (regex)
  "Does the previous line that is not an empty line begin with REGEX?"
  (save-excursion
    (and (previous-content-line)
         (line-begins regex))))

(defun previous-ends (regex)
  "Does the previous line that is not an empty line end with REGEX?"
  (save-excursion
    (and (previous-content-line)
         (line-ends regex))))


(defun indentation-of-previous-open ()
  "Need this procedure to find out how much to indent lines that starts with 'end'.
This procedure tries to understand nested 'if's and 'case's, although it's kind of difficult."
  (let ((helper
         (lambda (ends-seen)
           ;; `ends-seen` is used to count how many 'end's I've seen, to make sure
           ;; I get the right 'open'.
           (previous-content-line)
           (cond ((bobp) 0)
                 ((or (line-begins ".* := +if .*")
                      (line-begins ".*case .* of\\b")
                      (line-begins " *\\(then \\|else \\)?if\\b"))
                  (if (= ends-seen 0)
                      (find-indentation)
                    (apply helper (list (1- ends-seen)))))
                 ((line-begins " *end\\b") (apply helper (list (1+ ends-seen))))
                 (t (apply helper (list ends-seen)))))))
    (save-excursion (apply helper (list 0)))))

(defun case-clause-indentation ()
  "Need this procedure to find out how much to indent lines that contain ' ->'.
This procedure is not very robust as it doesn't understand nested case constructs in any way,
but nested case constructs are pretty rare. At least by using this procedure, I indent a lot
of lines correctly."
  (save-excursion
    (previous-content-line)
    (cond ((bobp) 0)
          ((line-begins ".*case .* of\\b") (+ (find-indentation) 2))
          ((line-begins ".* ->") (find-indentation))
          (t (case-clause-indentation)))))
  

(defun malang-indent-line ()
  "Indent the line at point. It's not perfect, it will fail in many cases, but it tries to be a little smart.
This procedure hasn't heard about tabs. It only knows spaces."
  (save-excursion (malang-indent-line-1))
  (if (empty-linep)
      (end-of-line)))

(defun malang-indent-line-1 ()
  (if (bobp)
      (indent-line-to 0)
    (let* ((base-indent (save-excursion (forward-line -1) (find-indentation)))
           (additional-indent
            (cond ((line-begins " *where *$") -9)
                  ((line-begins " *end\\b")
                   (- (indentation-of-previous-open) base-indent))

                  ((line-begins " *then\\b") 2)
                  
                  ((line-begins " *\\]") -2)
                  ((previous-ends "\\[ *") 2)

                  ((previous-begins " *\\(then\\|else\\) \\(if\\|case\\)\\b") 2)
                  ((previous-begins " *then\\b") (if (line-begins " *else\\b") 0 5))
                  ((line-begins " *else\\b") (if (previous-begins " *end\\b") 0 -5))
                  
                  ((previous-begins " *else\\b") 5)
                  ((previous-begins " *\\(if\\|case\\)\\b") 2)
                  ((previous-begins " *exposing\\b") 9)
                  ((line-begins ".* ->") (- (case-clause-indentation) base-indent))
                  ((previous-begins ".* ->")
                   (cond ((previous-begins ".* -> *$") 2)
                         (t (save-excursion
                              (previous-content-line)
                              (- (save-excursion (re-search-forward " ->")) (point) base-indent -1)))))
                  (t 0))))
      (indent-line-to (max 0 (+ base-indent additional-indent))))))

(defun malang-insert-lines (indentation lines)
  (insert "\n")
  (dolist (line lines)
    (indent-to indentation)
    (insert line)
    (insert "\n"))
  (delete-backward-char 1))


(defun malang-insert-func (prefix)
  (interactive "P")
  (let ((indentation (find-indentation))
        (lines (if (null prefix)
                   ;; Insert docstring if there is a prefix arg
                   (list "  "    "].")
                   (list "  \""    "  "    "  \"."    "]."))))
    (end-of-line)
    (just-one-space)
    (save-excursion
      (insert ":= [")
      (malang-insert-lines indentation lines)))
  (forward-line) (end-of-line)
  (unless (null prefix) (forward-line) (end-of-line)))


(defun insert-then-indent (string)
  (save-excursion
    (set-mark (point))
    (insert string)
    (indent-region (mark) (point))))

(defun malang-insert-if ()
  (interactive)
  (insert-then-indent "if \nthen .\nelse .\nend.")
  (end-of-line))

(defun malang-insert-case ()
  (interactive)
  (insert-then-indent "case ")
  (end-of-line)
  (insert-then-indent " of\n-> .\nend."))


(defvar malang-mode-hook nil)

(defvar malang-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c f") 'malang-insert-func)
    (define-key map (kbd "C-c c") 'malang-insert-case)
    (define-key map (kbd "C-c i") 'malang-insert-if)
    map)
  "Keymap for malang major mode")

(add-to-list 'auto-mode-alist '("\\.malang\\'" . malang-mode))

(defvar malang-keywords-regex
  (regexp-opt '("case" "of" "if" "then" "else" "end" "catch"
                "throw" "classified" "exposing" "where" "endify")))

(defvar malang-operators-regex
  (regexp-opt '(":=" "*" "**" "/" "-" "+" "%" ":" "::"
                ">" "<" ">=" "<=" "=" "!=" "~" "$")))

(defun malang-after-change (&rest args)
  (when (looking-back (format "\\b%s" malang-keywords-regex))
    (malang-indent-line)))

(defconst malang-font-lock-keywords
  (list
   (cons (format "\\<%s\\>" malang-keywords-regex) 'font-lock-keyword-face)
   (cons malang-operators-regex 'font-lock-function-name-face)
   '("->\\|<-" . font-lock-keyword-face)
   '("\\<yeah\\|nope\\>" . font-lock-builtin-face)
   '("\\<[a-z][A-Za-z0-9_]*\\>" . font-lock-constant-face)
   '("\\<[A-Z_][A-Za-z0-9_?']*\\>" . font-lock-variable-name-face)
   '("@" . font-lock-variable-name-face))
  "highlighting expressions for malang mode.")

(defvar malang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\? "w" st)
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?\- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for malang-mode")

(defun malang-mode ()
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table malang-mode-syntax-table)
  (use-local-map malang-mode-map)
  (set (make-local-variable 'indent-line-function) 'malang-indent-line)
  (add-hook 'after-change-functions 'malang-after-change nil t)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(malang-font-lock-keywords))
  (setq major-mode 'malang-mode)
  (setq mode-name "Malang")
  (run-hooks 'malang-mode-hook))

(provide 'malang-mode)
