(require 'quail)

(defun azbyn/quail-insert-key (key)

  (message (format "qual-insert key %s %s; %s"
                   key quail-current-key
                   (type-of key)
                   ))
  (setq quail-current-key ;; key)
        (concat quail-current-key  (char-to-string key)
                ));; (char-to-string last-command-event)))
  (or (catch 'quail-tag
        (quail-update-translation (quail-translate-key))
        t)
      ;; If someone throws for `quail-tag' by value nil, we exit from
      ;; translation mode.
      (setq quail-translating nil)))

(defun azbyn/quail-insert-key-simple (key)
  (setq quail-current-key (char-to-string key))
  (or (catch 'quail-tag
        (quail-update-translation (quail-translate-key))
        t)
      ;; If someone throws for `quail-tag' by value nil, we exit from
      ;; translation mode.
      (setq quail-translating nil)))

(defun azbyn/quail-start-translation (key)
  "Start translation of the typed character KEY by the current Quail package.
Return the input string."
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (and (integerp key)
               (assq (if (quail-kbd-translate)
                         (quail-keyboard-translate key) key)
                     (cdr (quail-map))))
          (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map (quail-translation-keymap))
             ;; (generated-events nil)     ;FIXME: What is this?
             (input-method-function nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)
        (setq quail-current-key ""
              quail-current-str ""
              quail-translating t)
        (if key (quail-add-unread-command-events key))
        (while quail-translating
          (set-buffer-modified-p modified-p)
          (quail-show-guidance)
          (let* ((prompt (if input-method-use-echo-area
                             (format "%s%s %s"
                                     (or input-method-previous-message "")
                                     quail-current-str
                                     quail-guidance-str)))
                 (keyseq (read-key-sequence prompt nil nil t))
                 ;; (cmd 'azbyn/quail-insert-key)
                 ;; (cmd (lookup-key (quail-translation-keymap) keyseq))
                 )
            (if nil ;; (if key
                  ;;   (and (commandp cmd) (not (eq cmd 'quail-other-command)))
                  ;; (eq cmd 'quail-self-insert-command))
                (progn
                  ;; (setq last-command-event (aref keyseq (1- (length keyseq)))
                  ;;       last-command this-command
                  ;;       this-command 'azbyn/quail-insert-key)
                  ;; (setq key t)
                  (condition-case err
                      (;call-interactively '
                       azbyn/quail-insert-key key)
                    (quail-error (message "%s" (cdr err)) (beep)))

                  (quail-terminate-translation);;TODO
                  ;; (setq quail-translating nil)
                  )
              ;; KEYSEQ is not defined in the translation keymap.
              ;; Let's return the event(s) to the caller.
              (quail-add-unread-command-events (this-single-command-raw-keys))
              (setq quail-translating nil))))
        (quail-delete-region)
        quail-current-str)

    ;; Since KEY doesn't start any translation, just return it.
    ;; But translate KEY if necessary.
    (if (quail-kbd-translate)
        (setq key (quail-keyboard-translate key)))
    (message "just key")
    (char-to-string key)
    ;; key

    ))


(defun azbyn/quail-input-method (key)
  (if nil ;; (or (and (or buffer-read-only
          ;;          (and (get-char-property (point) 'read-only)
          ;;               (get-char-property (point) 'front-sticky)))
          ;;      (not (or inhibit-read-only
          ;;               (get-char-property (point) 'inhibit-read-only))))
          ;; (and overriding-terminal-local-map
          ;;      ;; If the overriding map is `universal-argument-map', that
          ;;      ;; must mean the user has pressed 'C-u KEY'.  If KEY has a
          ;;      ;; binding in `universal-argument-map' just return
          ;;      ;; (list KEY), otherwise act as if there was no
          ;;      ;; overriding map.
          ;;      (or (not (eq (cadr overriding-terminal-local-map)
          ;;                   universal-argument-map))
          ;;          (lookup-key overriding-terminal-local-map (vector key))))
          ;; overriding-local-map)
      (list key)
    (quail-setup-overlays (quail-conversion-keymap))
    (with-silent-modifications
      (unwind-protect
          (let ((input-string (if (quail-conversion-keymap );;TODO just replace with the false?
                                  (quail-start-conversion key)

                                
                                (azbyn/quail-start-translation key))))
            (setq quail-guidance-str "")
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                (quail-input-string-to-events input-string))))
        (quail-delete-overlays)
        ;; Run this hook only when the current input method doesn't require
        ;; conversion.  When conversion is required, the conversion function
        ;; should run this hook at a proper timing.
        (unless (quail-conversion-keymap)
          (run-hooks 'input-method-after-insert-chunk-hook))))))

(defun azbyn/input-utf-char (chr)
  ;; (azbyn/quail-insert-key-simple chr)
  (azbyn/quail-input-method chr)
  
  )

(provide 'quail-redone)
