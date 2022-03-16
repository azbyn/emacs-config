(require 'haskell-mode)
(defvar lambda-mode-hook 'nil)

(defvar lambda-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "H-c") (lambda ()
    ;;                          (interactive)
    ;;                          ;;(save-buffer)
    ;;                          (compile (message "poetrizer -c \"%s\" %d"
    ;;                                            (buffer-file-name)
    ;;                                            (line-number-at-pos)))
    ;;                          ))
    map)
  "Keymap for Lambda major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lambda\\'" . lambda-mode))

(defconst lambda-font-lock-keywords
  (list

   '("\\('\\\\.+?'\\|'.'\\)" . font-lock-string-face)
   '("\\(\\\\\\|\\.\\|->\\)" . font-lock-keyword-face)
   '("\\<\\(let\\|in\\|include\\|notinline\\|testcase\\|typedef\\|newtype\\)\\>" . font-lock-keyword-face)
   '("\\(\\w+\\)" . font-lock-variable-name-face)
   ;; '("\\(\\d+\\)" . font-lock-)
   '("\\(\".+?\"\\)" . font-lock-string-face)
     )
  "Highlighting expressions for Lambda mode")


(defvar lambda-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; comments
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)

    ;; make symbols part of strings
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?_ "_" st)

    ;; ' to quote
    (modify-syntax-entry ?\' "'" st)
    
    ;; make \ punctuation
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?. "." st)

    
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)

    ;; "{}[]" not delimiters
    (modify-syntax-entry ?\[ "w" st)
    (modify-syntax-entry ?\] "w" st)
    (modify-syntax-entry ?\} "w" st)
    (modify-syntax-entry ?\{ "w" st)
    st)
  "Syntax table for lambda-mode")

;; (defvar lambda-mode-map)




(define-derived-mode lambda-mode prog-mode "Lambda"
  "Major mode for editing Lambda files."
  (set (make-local-variable 'font-lock-defaults) '(lambda-font-lock-keywords))
  (set-syntax-table lambda-mode-syntax-table)

  (highlight-numbers--turn-on)
  (rainbow-delimiters-mode-enable)

  ;; (linum-mode)
  ;; (line-number-mode 1)
  (setq prettify-symbols-alist '(("/\\" . ?Λ)
                                 ("\\" . ?λ)
                                 ;; (".\\" . "_")
                                 ))
  (setq-local tab-width 2)
  (setq-local comment-start "# "
              comment-start-skip "#+?\\s-*"
              comment-end "")
  (setq-local indent-tabs-mode nil)
  ;; (setq )
  ;;(flyspell-mode-on)
  ;;(spacemacs/toggle-spelling-checking-on)
  ;; (set-syntax-table lambda-mode-syntax-table)
  ;; :syntax-table (make-syntax-table)
  ;;(set (make-local-variable 'indent-line-function) 'lambda-indent-line)
  )


(provide 'lambda-mode)
