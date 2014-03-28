

(defvar malang-mode-hook nil)
(defvar malang-mode-map
  (let ((malang-mode-map (make-keymap)))
    malang-mode-map)
  "Keymap for malang major mode")

(add-to-list 'auto-mode-alist '("\\.malang\\'" . malang-mode))

(defconst malang-font-lock-keywords
  (list
   '("\\(\\<\\(case\\|of\\|end\\)\\>\\|->\\)" . font-lock-keyword-face)
   '("\\<\\([a-z][A-Za-z0-9_]*\\)\\>" . font-lock-constant-face)
   '("\\(\\<\\([A-Z_][A-Za-z0-9_]*\\)\\>\\|@\\)" . font-lock-variable-name-face))
  "highlighting expressions for malang mode.")

(defvar malang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for malang-mode")

(defun malang-mode ()
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table malang-mode-syntax-table)
  (use-local-map malang-mode-map)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(malang-font-lock-keywords))
  (setq major-mode 'malang-mode)
  (setq mode-name "Malang")
  (run-hooks 'malang-mode-hook))

(provide 'malang-mode)