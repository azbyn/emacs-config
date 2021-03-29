(defvar poetry-mode-hook 'nil)

(defvar poetry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "H-c") (lambda ()
                             (interactive)
                             ;;(save-buffer)
                             (compile (message "poetrizer -c \"%s\" %d"
                                               (buffer-file-name)
                                               (line-number-at-pos)))
                             ))
    map)
  "Keymap for Poetry major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ptr\\'" . poetry-mode))

(defconst poetry-font-lock-keywords-1
  (list
   '("\\<\\(paper\\|epilogueSpace\\|l\\(eft\\|ineStrech\\)\\|right\\|t\\(op\\|itleS\\(ize\\|pacing\\)\\)\\|bottom\\|s\\(ize\\|ubtitleSize\\|tropheSpacing\\)\\|font\\|colS\\(pacing\\|ize\\)\\|f\\(irstMessageSize\\|ootnote\\(Size\\|Font\\)\\)\\)\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face
     ))
  "Minimal highlighting expressions for Poetry mode")

(defconst poetry-font-lock-keywords-2
  (append poetry-font-lock-keywords-1
          (list
           '("\\<\\(SET\\|PREAMBLE\\|MOTTO\\|EPILOG\\|F\\(IRST_MSG\\|OOTNOTE\\)\\|END_\\(MOTTO\\|EPILOG\\|F\\(IRST_MSG\\|OOTNOTE\\)\\)\\)\\>" . font-lock-keyword-face)
           '("\\<\\(TRUE\\|FALSE\\)\\>" . font-lock-constant-face)))
  "Additional Keywords to highlight in Poetry mode")
(set (make-local-variable 'comment-start) "#")
(defconst poetry-font-lock-keywords-3
  (append poetry-font-lock-keywords-2
          (list
           '("\\\\\\w+" . font-lock-builtin-face)
           ;;'("\\{\\|\\}" . font-lock-builtin-face)
           ))
  "Balls-out highlighting in Poetry mode")

(defvar poetry-font-lock-keywords poetry-font-lock-keywords-3
  "Default highlighting expressions for Poetry mode")

(defvar poetry-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for wpdl-mode")

(defvar poetry-mode-map)

(setq comment-start "# "
      comment-start-skip "#+?\\s-*")


(define-derived-mode poetry-mode fundamental-mode "Poetry"
  "Major mode for editing Poetry files."
  (set (make-local-variable 'font-lock-defaults) '(poetry-font-lock-keywords))

  ;;(flyspell-mode-on)
  ;;(spacemacs/toggle-spelling-checking-on)
  ;; (set-syntax-table poetry-mode-syntax-table)
  ;; :syntax-table (make-syntax-table)
  ;;(set (make-local-variable 'indent-line-function) 'poetry-indent-line)
  )


(provide 'poetry-mode)
