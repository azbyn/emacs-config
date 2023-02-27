;; (local-set-key (kbd "M-c") 'org-latex-export-to-pdf)
;; * lexical bindings
;; We have to leave this as the first line
(setq lexical-binding t)
(defconst azbyn/windows-mode (string-equal system-type "windows-nt"))
(defvar azbyn/height (cond
                      (azbyn/windows-mode 117)
                      ((string-equal (system-name) "tadeusz") 100)
                      (t 107)))

(defvar azbyn/font (if azbyn/windows-mode "Consolas" "DejaVu Sans Mono"))

(set-fontset-font t '(?Ꙁ . ?ꙮ) "Fira Slav")
(set-fontset-font t '(?꙯ . ?ꚟ) "Fira Slav")
(set-fontset-font t '(?ⷠ . ?ⷿ) "Fira Slav")
(set-fontset-font t '(?ᲀ . ?ᲈ) "Fira Slav")
(set-fontset-font t '(?Ѡ . ?҉) "Fira Slav")
(set-fontset-font t ?̾  "Fira Slav")
(set-fontset-font t ?̆  "Fira Slav")
(set-fontset-font t ?і "Fira Slav")

(set-fontset-font t '(?Ѐ . ?Џ) "Fira Slav")
(set-fontset-font t '(?ѐ . ?џ) "Fira Slav")

(set-fontset-font t '(?А . ?я) "Fira Slav")

;; (defun azbyn/transliterate-face ()
;;   (interactive)

;;   (font-lock-add-keywords
;;    'org-mode `(("\\(ш\\)"
;;                 (0 (progn (compose-region (match-beginning 1)
;;                                           (match-end 1) "ș")
;;                           nil)))))
;;   )


(use-package hide-mode-line
  :ensure t)


(defun azbyn/redo-cyrillic-font ()
  (interactive)
  (set-fontset-font nil '(?Ѐ . ?ӿ) "Monomakh Unicode")
  (set-fontset-font nil '(?Ꙁ . ?ꚟ) "Monomakh Unicode")
  (set-fontset-font nil '(?ᲀ . ?ᲈ) "Monomakh Unicode")
  )
(defun azbyn/redo-cyrillic-font-ponomar ()
  (interactive)
  (set-fontset-font nil '(?Ѐ . ?ӿ) "Ponomar Unicode")
  (set-fontset-font nil '(?Ꙁ . ?ꚟ) "Ponomar Unicode")
  (set-fontset-font nil '(?ᲀ . ?ᲈ) "Ponomar Unicode")
  )

(define-minor-mode azbyn/cyrillic-preview-mode
  "Use a different font for the current frame."
  :init-value nil
  :global nil

  (let* ((enabled azbyn/cyrillic-preview-mode)
         (frame (selected-frame))
         (font-name (if enabled
                        "Monomakh Unicode"
                      azbyn/font
                      ))
         (font-name-cyr (if enabled
                            "Monomakh Unicode"
                          "Fira Slav"
                          ))
         (height (if enabled 160 100))
        )
    (select-frame frame)
    (set-frame-font font-name)

    (set (make-local-variable 'scroll-margin) (if enabled 1 3))

    (message font-name-cyr)
    ;; (format "%s %d" font-name font-size))

    ;; (set-fontset-font nil '(?Ѐ . ?ӿ) "Monomakh Unicode")
    ;; (set-fontset-font nil '(?Ꙁ . ?ꚟ) "Monomakh Unicode")
    ;; (set-fontset-font nil '(?ᲀ . ?ᲈ) "Monomakh Unicode")
    
    (set-fontset-font nil '(?Ѐ . ?ӿ) font-name-cyr);cyrillic
    (set-fontset-font nil '(?ⷠ . ?ⷿ) font-name-cyr);extended a
    (set-fontset-font nil '(?Ꙁ . ?ꚟ) font-name-cyr);extended b
    (set-fontset-font nil '(?ᲀ . ?ᲈ) font-name-cyr);extended c
    ;; (text-scale-increase 2)

    (set-face-attribute 'default frame :height height)
    (setq line-spacing (if enabled 0.3 0))
    ;; (if enabled
    ;;     (turn-on-hide-mode-line-mode)
    ;;   (turn-off-hide-mode-line-mode))

    )
  )

(add-hook 'quail-activate-hook 'azbyn/old-romanian-extra-enable)
(add-hook 'quail-deactivate-hook 'azbyn/old-romanian-extra-disable)

(setq-default
 quail-simple-translation-keymap
 (let ((map (make-keymap))
       (i 0))
   (while (< i ?\ )
     (define-key map (char-to-string i) 'quail-other-command)
     (setq i (1+ i)))
   (while (< i 127)
     (define-key map (char-to-string i) 'quail-self-insert-command)
     (setq i (1+ i)))
   (setq i 128)
   (while (< i 256)
     (define-key map (vector i) 'quail-self-insert-command)
     (setq i (1+ i)))
   (define-key map "\177" 'quail-delete-last-char)
   ;; (define-key map [delete] 'quail-delete-last-char)
   (define-key map [backspace] 'quail-delete-last-char)
   ;;(let ((meta-map (make-sparse-keymap)))
   ;;(define-key map (char-to-string meta-prefix-char) meta-map)
   ;;(define-key map [escape] meta-map))
   map))


(defun azbyn/cyrillic-preview-create ()
  (interactive)
  (let ((frame (make-frame)))
    (select-frame frame)
    (azbyn/cyrillic-preview-mode 1))
  )

(require 'flycheck)

(flycheck-define-checker rom-cyr-check-accents
  "Check romanian cyrillic diacritics"
  :command ("py" "/home/azbyn/Projects/Disertatie/checker/checker.py"
            source
            ;; We must stay in the same directory to resolve @include
            ;; source-inplace
            )
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": error: " (message) line-end))
  :modes (text-mode org-mode))

(add-hook 'org-mode-hook 'flycheck-mode)

;; * setup the path
(when (file-exists-p "~/.emacs.d/lisp/")
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  ;;add all subdirs from ~/.emacs.d/lisp/
  (let ((default-directory  "~/.emacs.d/lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

(when (file-exists-p "~/.emacs.d/themes/")
  (add-to-list 'load-path "~/.emacs.d/themes/")
  ;;add all subdirs from ~/.emacs.d/lisp/
  (let ((default-directory  "~/.emacs.d/themes/"))
    (normal-top-level-add-subdirs-to-load-path)))

;; * old - emacs ng thing
;; emacs-ng throws A LOT of warnings, most of which are the package
;; writer's fault like:
;; lsp-java.el:440:1: Warning: custom-declare-variable `lsp-java-import-gradle-home' docstring wider than 80 characters Disable showing Disable logging

;; and the warning buffer pops up like every other second.

;; so:
;; (setq warning-minimum-level :emergency)

;; * compilation thing
(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

;; * Windows mode
;; (setq-default azbyn/windows-mode (string-equal system-type "windows-nt"))
(when azbyn/windows-mode
  (setenv "PATH" (concat "D:\\cygwin\\bin"
                         path-separator
                         (getenv "PATH")))
  ;;not great as it's not defined on the first run ;_;
  (when (boundp 'vdiff-diff-algorithms)
    (setf (alist-get 'diff vdiff-diff-algorithms) "D:\\cygwin\\bin\\diff.exe -u"))
  ;; bat file
  ;; set PATH=%PATH%;C:\LegacyApp\Cygwin\3.0.7\bin
  ;; wherever\emacs.exe

  ;; comment me and remember to change init.el
  ;;(setq default-frame-alist
  ;;      (append default-frame-alist '((font . "Consolas")
  ;;                                    (height . 117))))
  ;; </>
  (setq shell-file-name "cmdproxy.exe")
  (setq w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-pass-apps-to-system nil
        w32-lwindow-modifier 'meta ;; Left Windows key
        w32-rwindow-modifier 'meta ;; Right Windows key
        ;; w32-apps-modifier 'hyper ;; Menu key
        )
  ;; (w32-register-hot-key [s-])
  )
;; * Theme
;; ** colors
(require 'base16-azbyn-google-dark-theme)
(defconst azbyn-base16-colors base16-azbyn-google-dark-colors)

(defconst base16-col-base00 (plist-get azbyn-base16-colors :base00))
(defconst base16-col-base01 (plist-get azbyn-base16-colors :base01))
(defconst base16-col-base02 (plist-get azbyn-base16-colors :base02))
(defconst base16-col-base03 (plist-get azbyn-base16-colors :base03))
(defconst base16-col-base04 (plist-get azbyn-base16-colors :base04))
(defconst base16-col-base05 (plist-get azbyn-base16-colors :base05))
(defconst base16-col-base06 (plist-get azbyn-base16-colors :base06))
(defconst base16-col-base07 (plist-get azbyn-base16-colors :base07))
(defconst base16-col-base08 (plist-get azbyn-base16-colors :base08))
(defconst base16-col-base09 (plist-get azbyn-base16-colors :base09))
(defconst base16-col-base0A (plist-get azbyn-base16-colors :base0A))
(defconst base16-col-base0B (plist-get azbyn-base16-colors :base0B))
(defconst base16-col-base0C (plist-get azbyn-base16-colors :base0C))
;; (defconst base16-col-base0C )
(defconst base16-col-base0D (plist-get azbyn-base16-colors :base0D))
(defconst base16-col-base0E (plist-get azbyn-base16-colors :base0E))
(defconst base16-col-base0F (plist-get azbyn-base16-colors :base0F))

(defvaralias 'base16-col-black   'base16-col-base00)
(defvaralias 'base16-col-gray    'base16-col-base05)
(defvaralias 'base16-col-white   'base16-col-base07)
(defvaralias 'base16-col-red     'base16-col-base08)
(defvaralias 'base16-col-orange  'base16-col-base09)
(defvaralias 'base16-col-yellow  'base16-col-base0A)
(defvaralias 'base16-col-green   'base16-col-base0B)
(defvaralias 'base16-col-cyan    'base16-col-base0C)
(defvaralias 'base16-col-blue    'base16-col-base0D)
(defvaralias 'base16-col-magenta 'base16-col-base0E)

;; ** the base16 theme
(add-to-list 'custom-theme-load-path "~/.emacs/themes")

(use-package base16-theme
  :ensure t
 ;; :init
 ;; (setq-default base16-google-dark-colors
 ;;               `(:base00 ,base16-col-base00
 ;;                 :base00 ,base16-col-base01
 ;;                 :base02 ,base16-col-base02
 ;;                 :base03 ,base16-col-base03
 ;;                 :base04 ,base16-col-base04
 ;;                 :base05 ,base16-col-base05
 ;;                 :base06 ,base16-col-base06
 ;;                 :base07 ,base16-col-base07
 ;;                 :base08 ,base16-col-base08
 ;;                 :base09 ,base16-col-base09
 ;;                 :base0A ,base16-col-base0A
 ;;                 :base0B ,base16-col-base0B
 ;;                 :base0C ,base16-col-base0C
 ;;                 :base0D ,base16-col-base0D
 ;;                 :base0E ,base16-col-base0E
 ;;                 :base0F ,base16-col-base0F))
  :config
  (setq-default base16-theme-256-color-source "base16-shell")
  (load-theme 'base16-azbyn-google-dark t)
  (unless (display-graphic-p)
    '(set-face-background 'default "unspecified-bg" (selected-frame)))
  )

(defun azbyn/ah-my-eyes ()
  (interactive)
  (load-theme 'base16-azbyn-google-light t))

(defun azbyn/light-theme ()
  (interactive)
  (azbyn/ah-my-eyes))
(defun azbyn/dark-theme ()
  (interactive)
  (load-theme 'base16-azbyn-google-dark t))


;; ** ansi colors
(setq-default ansi-color-names-vector
              (vector base16-col-black
                      base16-col-red
                      base16-col-green
                      base16-col-yellow
                      base16-col-blue
                      base16-col-magenta
                      base16-col-blue ;; looks better than cyan
                      base16-col-gray
                      ))

;; ** highlight numbers
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; ** highlight escape sequences
  (use-package highlight-escape-sequences
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'hes-mode)
    (put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
    (put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face))

;; ** highlight ansi in compilation
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; ** highlight TODO and NOTE
(use-package hl-todo
  :ensure t)
(global-hl-todo-mode)

;; * diminish
(use-package diminish
  :ensure t
  :config
  (diminish 'whitespace-mode)
  (diminish 'flyspell-mode)
  (diminish 'yas-minor-mode)
  (diminish 'yas-mode)
  (diminish 'eldoc-mode)
  (diminish 'hs-minor-mode)
  (diminish 'flyspell-mode "s")
  (diminish 'flymake-mode "fm")
  (diminish 'wucuo-mode "wu")
  (diminish 'flycheck-mode "fc")
  (diminish 'defining-kbd-macro "Macro"))
;; * flyspell
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en"))
;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=ro"))

;; * Basic packages
;; ** yasnippet
(use-package yasnippet
  :ensure t)

;; ** flycheck-mode
(use-package flycheck
  :ensure t)
;; ** which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; ** ibuffer
(global-set-key (kbd "C-x b") 'ibuffer)

;; ** Rainbow
;; Preview the color when you encounter a hex code
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :init
  (add-hook 'org-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  )
;; only do this for #rrggbb, not #rgb and others
;; (#define woulda've ben broken)
;;I don't care about other more exotic formats
(setq rainbow-hexadecimal-colors-font-lock-keywords
      '(("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)\\{2\\}\\)"
         (1 (rainbow-colorize-itself 1)))))
;; I know what "red" looks like, I don't need it highlighed
(setq rainbow-html-colors nil)
(setq rainbow-x-colors nil)
(setq rainbow-latex-colors nil)
(setq rainbow-r-colors nil)


;; ** expand region
  (use-package expand-region
    :ensure t
    :bind (("M-s" . er/expand-region)
           ("M-S" . er/contract-region)))

;; ** common lisp stuff
(require 'cl-lib)

;; ** find file in project
(use-package find-file-in-project
  :ensure t
  :config
  (setq-default ffip-ignore-filenames
            (remove "*.d" ffip-ignore-filenames)))
;; ** ranger
(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))
;; ** persp mode todo
(use-package persp-mode
  :ensure t)

;; ** popwin
;; no more of ruining an entire window split with some buffers
(use-package popwin
   :ensure t
   :config
   (popwin-mode 1))

;; ** smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config))
(add-hook 'prog-mode-hook 'smartparens-mode)

;; ** htmlize
;; for org mode conversions
(use-package htmlize
  :ensure t)

;; * Better defaults
;; ** no scroll bars and stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; ** no more typing =yes=
(defalias 'yes-or-no-p 'y-or-n-p)

(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; ** better scrolling
(setq scroll-conservatively 999
      scroll-margin 3
      scroll-step 1)


;; ** no bell
(setq ring-bell-function 'ignore)

;; ** highlight current line
(global-hl-line-mode t)

;; ** lambda becomes λ among other things
(global-prettify-symbols-mode t)

;; ** parens
(show-paren-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; ** open compressed files
(auto-compression-mode t)

;; ** utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ** line numbers
(if (< emacs-major-version 26)
    (defun display-line-numbers-mode()
      (interactive)
      (linum-mode)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
;; (use-package nlinum
;;   :ensure t)
;; (defun my-nlinum-mode-hook ()
;;   (when nlinum-mode
;;     (setq-local nlinum-format
;;                 (concat " %" (number-to-string
;;                              ;; Guesstimate number of buffer lines.
;;                              (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
;;                         "d"))))
;;(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)
;;(defun my-nlinum-mode-hook ()
;;  (when nlinum-mode
;;   (setq-local nlinum-format
;;                (concat " %" (number-to-string
;;                             ;; Guesstimate number of buffer lines.
;;                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))))))

;; (add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

;;  (add-hook 'prog-mode-hook 'nlinum-mode)
;;  (add-hook 'text-mode-hook 'nlinum-mode)

;;(global-display-line-numbers-mode)

;; ** disable line numbers for some modes
;;  (defun disable-line-numbers (&optional dummy)
;;    (display-line-numbers-mode -1))
;;  ;;(add-hook 'neo-tree-mode-hook 'disable-line-numbers)
;;  (add-hook 'neo-after-create-hook 'disable-line-numbers)
;;  (add-hook 'dashboard-mode-hook 'disable-line-numbers)
;;  (add-hook 'dired-mode-hook 'disable-line-numbers)
;; ** electric pairs
;; (setq electric-pair-pairs '(
                             ;; (?\{ . ?\})
                             ;; (?\( . ?\))
                             ;; (?\[ . ?\])
                             ;; (?\" . ?\")
  ;;                            ))
  ;; (  electric-pair-mode t)

;; ** don't ask about following symlinks
(setq vc-follow-symlinks t)

;; ** ask for confirmation on close
(setq confirm-kill-emacs 'y-or-n-p)

;; ** dired directories first
(setq dired-listing-switches "-al --group-directories-first")


;; * non-melpa packages
;; ** move line
(require 'move-lines)
(move-lines-binding)
;; ** help plus
(require 'help-fns+)
;; * Whitespace related stuff
;; ** no tabs
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
;; ** show tabs and other whitespace
(setq-default whitespace-style '(face
                                 trailing
                                 tabs
                                 ;;spaces
                                 space-before-tab
                                 ;;space-after-tab
                                 tab-mark
                                 ;;space-mark
                                 ;;lines-tail
                                 ))
(defun diminished-whitespace-mode ()
  (interactive)
  (whitespace-mode)
  (diminish 'whitespace-mode))
(add-hook 'prog-mode-hook 'diminished-whitespace-mode)

(setq-default whitespace-line-column 180)

;; ** 4 space indents
(setq tab-width 4)

;; * Terminal and eshell
;; ** Use zsh by default
(unless azbyn/windows-mode
  (defadvice ansi-term (before force-bash)
    (interactive (list "/usr/bin/zsh")))

  (ad-activate 'ansi-term))
;; ** aliases
(defalias 'e 'find-file)
(defalias 'ef 'find-file)
(defalias 'es 'eshell)
(defalias 'eo 'find-file-other-window)

;; ** xterm color
;; (use-package xterm-color
;;   :ensure t
;;   :config
;;   (require 'eshell) ; or use with-eval-after-load

;;   (add-hook 'eshell-before-prompt-hook
;;             (lambda ()
;;               (setq xterm-color-preserve-properties t)))
;;   (unless (boundp 'eshell-output-filter-functions)
;;     (defvar eshell-preoutput-filter-functions nil))
;;   ;;(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;   ;;(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
;;   (setq-default 'eshell-preoutput-filter-functions 'xterm-color-filter)

;;   (setq xterm-color-names
;;         (vector base16-col-base00 ; black
;;          base16-col-base08 ; red
;;          base16-col-base0B ; green
;;          base16-col-base0A ; yellow
;;          base16-col-base0D ; blue
;;          base16-col-base0C ; magenta
;;          base16-col-base0E ; cyan
;;          base16-col-base05 ; white
;;          ))
;;   (setq xterm-color-names-bright
;;         (vector base16-col-base03 ; black
;;          base16-col-base08 ; red
;;          base16-col-base0B ; green
;;          base16-col-base0A ; yellow
;;          base16-col-base0D ; blue
;;          base16-col-base0E ; magenta
;;          base16-col-base0C ; cyan
;;          base16-col-base07 ; white
;;         ))
;;   (setenv "TERM" "xterm-256color")
;;   )
;; ** fix my bindings
(require 'eshell)
(add-hook 'eshell-mode-hook 'company-mode);; auto-complete-mode)

;; ** fish completion
(unless azbyn/windows-mode
  (use-package fish-completion
    :ensure t
    :config
    (when (and (executable-find "fish")
             (require 'fish-completion nil t))
      (add-hook 'eshell-mode-hook 'fish-completion-mode))))

;; * window numbering
  (use-package window-numbering
    :ensure t
    :init (window-numbering-mode))

;; * Dashboard
;; ** no more startup message
(setq inhibit-startup-message t)
;; ** install
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;;    (setq dashboard-startup-banner "~/.emacs.d/img/dashLogo.png")
  (setq dashboard-items '((recents  . 7)
                          (projects . 5)))
  (setq dashboard-banner-logo-title ""))
;; * projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode 1))
;; * spaceline
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  ;;(setq spaceline-buffer-encoding-abbrev-p nil)
  ;;(setq spaceline-line-column-p nil)
  ;;(setq spaceline-line-p nil)
  (setq powerline-default-separator (quote arrow))
  (setq spaceline-window-numbers-unicode t)
  (spaceline-toggle-evil-state-off)
  (spaceline-toggle-persp-name-on)
  (spaceline-toggle-window-number-on)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))
(unless (display-graphic-p)
  (setq spaceline-window-numbers-unicode nil))

(spaceline-define-segment azbyn-lines
  "the number of lines"
  (if (eq major-mode 'pdf-view-mode)
      (spaceline--pdfview-page-number)
    (let* ((total-lines (save-excursion
                          (goto-char (point-max))
                          (format-mode-line "%l")))
           (line-num (format-mode-line "%l"))
           (perc (/ (* 100 (string-to-number line-num))
                    (string-to-number total-lines)))
           (col (format-mode-line "%2c")));;%2C
      (format "%s:%s | %3d%%%%" line-num col perc);; total-lines)
      )))

(spaceline-compile
  ;; left side
  '(((persp-name
      workspace-number
      window-number)
     :fallback evil-state
     :face highlight-face
     :priority 100)
    (anzu :priority 95)
    auto-compile
    ((buffer-modified buffer-size buffer-id remote-host)
     :priority 98)
    (major-mode :priority 79)
    (process :when active)
    ((flycheck-error flycheck-warning flycheck-info)
     :when active
     :priority 89)
    (minor-modes :when active
                 :priority 9)
    (mu4e-alert-segment :when active)
    (erc-track :when active)
    ;;(version-control :when active
    ;;                 :priority 78)
    (org-pomodoro :when active)
    (org-clock :when active)
    nyan-cat)
  ;; right side
  '(which-function
    (python-pyvenv :fallback python-pyenv)
    (purpose :priority 94)
    (battery :when active)
    (selection-info :priority 95)
    input-method
    ((buffer-encoding-iabbrev
      point-position
      ;;line-column
      ;;num-lines
      azbyn-lines
      )
     :separator " | "
     :priority 96)
    (global :when active)
    ;;(buffer-position :priority 99)
    ;;(hud :priority 99)
    ))
;;(setq line-number-mode t)
;;(setq column-number-mode t

;; * magit
(use-package magit
  :ensure t)

;; * neotree
(use-package neotree
  :ensure t
  :bind ("H-t" . 'neotree-toggle))

;; * outshine (org outside org)
(use-package outshine
  :ensure t)

(defvar outline-minor-mode-prefix "\M-#")
(add-hook 'prog-mode-hook 'outshine-mode)


(define-key outshine-mode-map (kbd "<M-S-up>") nil)
(define-key outshine-mode-map (kbd "<M-S-down>") nil)
(define-key outshine-mode-map (kbd "<M-up>") nil)
(define-key outshine-mode-map (kbd "<M-down>") nil)

(define-key outshine-mode-map (kbd "<M-left>") 'outline-hide-entry)
(define-key outshine-mode-map (kbd "<M-right>") 'outline-show-entry)

(define-key outshine-mode-map (kbd "<M-S-right>") 'outline-show-all)
(define-key outshine-mode-map (kbd "<M-S-left>") 'outline-hide-body)

(define-key outshine-mode-map (kbd "<M-C-right>") 'outline-show-all)
(define-key outshine-mode-map (kbd "<M-C-left>") 'outline-hide-body)

;; * Org mode
;; ** macro for emacs-lisp
;; (local-set-key (kbd "M-c") 'org-latex-export-to-pdf)
(if (version< org-version "9.2")
    (add-to-list 'org-structure-template-alist
                 '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist
               '("p" . "src python"))
  (add-to-list 'org-structure-template-alist
               '("py" . "src python")))
;; ** bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; ** use the same window for =C-c '=
(setq org-src-window-setup 'current-window)

;; * Custom functions
;; ** sudo edit
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

  With a prefix ARG prompt for a file to visit.
  Will also prompt for a file to visit if current
  buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
;; ** reload config
(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (save-some-buffers)
  (load-file (expand-file-name "~/.emacs.d/config/config.el"))
  (load-file (expand-file-name "~/.emacs.d/config/keybindings.el")))

;; ** edit config
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config/config.el"))
(defun keybindings-visit ()
  (interactive)
  (find-file "~/.emacs.d/config/keybindings.el"))
(defun keybindings-visit-readonly ()
  (interactive)
  (find-file-read-only "~/.emacs.d/config/keybindings.el"))
(defun config-visit-readonly ()
  (interactive)
  (find-file-read-only "~/.emacs.d/config/config.el"))

;; ** split and follow
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
;; ** smarter paste
(defun azbyn/is-image (str)
  (or (string-prefix-p "\x89PNG" str)
      (string-prefix-p "\xff\xd8\xff" str); jpg
      ))

(defadvice yank (around yank-no-binary activate)
  "Normal yank breaks undo-tree if we paste a png by mistake, so we fix that."
  (unless (and (azbyn/is-image (current-kill 0))
               (not (y-or-n-p "Clipboard contains an image. Continue?")))
    ad-do-it
    ))

(defun azbyn/paste ()
  (interactive "")
  (let ((el (first kill-ring)))
    (when (cl-search "\n" el)
      (end-of-line)
      (newline))
    (yank)
    (delete-char 1)
    (backward-char)))

(defun azbyn/paste-before ()
  (interactive "")
  (let ((el (first kill-ring)))
    (when (cl-search "\n" el)
      ;;(forward-line -1)
      (beginning-of-line))
    (yank)))
;; ** previous buffer
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
;; ** kill-whole-word
(defun daedreth/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
;; ** word and subword movement
(defun azbyn/subword-char-type (c)
  (let ((type (get-char-code-property c 'general-category)))
    (if (member type '(Lu Lt))
        ?U ;;u for uppercase
      (string-to-char (symbol-name type)))))

(defun azbyn/char-type (c)
  (if (not c)
      ?Z;;z of null
    (if (member c '(?\( ?\)))
        ?\( ;separate category for parens
      ;; can return (the first letter of)
      ;;Letter, Mark, Number, Punctuation, Symbol, Separator, C (other)
      (let ((type (get-char-code-property c 'general-category)))
        ;;make digits, combining characters and _ behave like letters
        (if (or (equal type 'Nd) (equal type 'Mn) (equal c ?_))
            ?L
            (string-to-char (symbol-name type)))))))
(defun azbyn/elisp-char-type (c)
  (if (member c '(?- ?/))
      ?L ;make - and / a leter
    (azbyn/char-type c)))
(defvar azbyn/char-type-function 'azbyn/char-type)

(setq-local azbyn/char-type-function 'azbyn/elisp-char-type)

;;TODO add a skip spaces?
(defun azbyn/word-begin-impl (char-type-fun move-fun get-char-fun)
  (cl-flet ((char-type (c)
                       (cond
                        ((equal c 10) 'newline)
                        (t (funcall char-type-fun c)))))
    (let ((initial-type (char-type (funcall get-char-fun))))
      (if (equal initial-type 'newline)
          (funcall move-fun)
        (unless (equal (funcall get-char-fun) ?\ )
          (while (equal (char-type (funcall get-char-fun)) initial-type)
            (funcall move-fun)))
        (while (equal (funcall get-char-fun) ?\ );;space
          (funcall move-fun))))))

(defun azbyn/word-end-impl (char-type-fun move-fun get-char-fun)
  (cl-flet ((char-type (c)
                       (cond
                        ((equal c 10) 'newline)
                        (t (funcall char-type-fun c)))))
    (let ((initial-type (char-type (funcall get-char-fun))))
      (while (equal (funcall get-char-fun) ?\ );;space
        (funcall move-fun))
      (if (equal initial-type 'newline)
          (funcall move-fun)
        (unless (equal (funcall get-char-fun) ?\ )
          (while (equal (char-type (funcall get-char-fun)) initial-type)
            (funcall move-fun))
          ;;(while (equal (funcall get-char-fun) ?\ );;space
          ;; (funcall move-fun))
          )))))

(defun azbyn/forward-word-begin ()
  (interactive)
  (azbyn/word-begin-impl azbyn/char-type-function 'forward-char 'char-after))
(defun azbyn/forward-word-end ()
  (interactive)
  (azbyn/word-end-impl azbyn/char-type-function 'forward-char 'char-after))
(defun azbyn/backward-word-end ()
  (interactive)
  (azbyn/word-begin-impl azbyn/char-type-function 'backward-char 'char-before))
(defun azbyn/backward-word-begin ()
  (interactive)
  (azbyn/word-end-impl azbyn/char-type-function 'backward-char 'char-before))

(defun azbyn/forward-subword-begin ()
  (interactive)
  (when (member (get-char-code-property (char-after) 'general-category)
                '(Lu Lt))
    (forward-char))
  (azbyn/word-begin-impl 'azbyn/subword-char-type 'forward-char 'char-after))
(defun azbyn/forward-subword-end ()
  (interactive)
  (when (member (get-char-code-property (char-after) 'general-category)
                '(Lu Lt))
    (forward-char))
  (azbyn/word-end-impl 'azbyn/subword-char-type 'forward-char 'char-after))
(defun azbyn/backward-subword-end ()
  (interactive)
  (azbyn/word-begin-impl 'azbyn/subword-char-type 'backward-char 'char-before)
  (when (member (get-char-code-property (char-before) 'general-category)
                '(Lu Lt))
    (backward-char)))
(defun azbyn/backward-subword-begin ()
  (interactive)
  (azbyn/word-end-impl 'azbyn/subword-char-type 'backward-char 'char-before)
  (when (member (get-char-code-property (char-before) 'general-category)
                '(Lu Lt))
    (backward-char)))

(defun azbyn/delete-one-char ()
  (interactive)
  (delete-char 1))
(defun azbyn/delete-one-char-backward ()
  (interactive)
  (delete-char -1))

(defun azbyn/kill-word ()
  (interactive)
  (azbyn/word-end-impl azbyn/char-type-function 'azbyn/delete-one-char 'char-after))
(defun azbyn/kill-subword ()
  (interactive)
  (when (member (get-char-code-property (char-after) 'general-category)
                '(Lu Lt))
    (delete-char 1))
  (azbyn/word-end-impl 'azbyn/subword-char-type 'azbyn/delete-one-char 'char-after))

(defun azbyn/kill-word-backward ()
  (interactive)
  (azbyn/word-end-impl azbyn/char-type-function 'azbyn/delete-one-char-backward 'char-before))
(defun azbyn/kill-subword-backward ()
  (interactive)
  ;; (when (member (get-char-code-property (char-after) 'general-category)
  ;;               '(Lu Lt))
  ;;   (delete-char 1))
  (azbyn/word-end-impl 'azbyn/subword-char-type 'azbyn/delete-one-char-backward 'char-before))


(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local azbyn/char-type-function 'azbyn/elisp-char-type)))
;; ** copy/kill-*-or-region
(defun azbyn/copy-to-eol ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point)
      (point-at-eol))))
  (message "copied to eol"))
(defun azbyn/copy-to-eol-or-region ()
  (interactive)
  (if mark-active
      (call-interactively 'kill-ring-save)
    (azbyn/copy-to-eol)))

(defun azbyn/kill-to-eol-or-region ()
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (kill-line)))
(defun azbyn/kill-whole-line-or-append-region ()
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    ;;(append-next-kill) ;;TODO
    (kill-whole-line)))
(defun azbyn/delete-char-or-region ()
  (interactive)
  (if mark-active
      (call-interactively 'delete-region)
    (delete-char 1)))
;; ** copy word
(defun azbyn/copy-whole-subword()
  (interactive)
  (save-excursion
    (forward-char)
    (let ((val (buffer-substring
                (azbyn/get-point 'azbyn/backward-subword-begin)
                (azbyn/get-point 'azbyn/forward-subword-end))))
      (message "copied %s" val)
      (kill-new val)
      )))
(defun azbyn/copy-whole-word()
  (interactive)
  (save-excursion
    (forward-char)
    (let ((val (buffer-substring
                (azbyn/get-point 'azbyn/backward-word-begin)
                (azbyn/get-point 'azbyn/forward-word-end))))
      (message "copied %s" val)
      (kill-new val)
      )))

(defun azbyn/kill-whole-word()
  (interactive)
  (forward-char)
  (kill-region (azbyn/get-point 'azbyn/backward-word-begin)
               (azbyn/get-point 'azbyn/forward-word-end)))

(defun azbyn/kill-whole-subword()
  (interactive)
  (forward-char)
  (kill-region (azbyn/get-point 'azbyn/backward-subword-begin)
               (azbyn/get-point 'azbyn/forward-subword-end)))
;; ** nicer delete
(defun get-deletion-count (arg)
  "Return the amount of spaces to be deleted, ARG is indentation border."
  (if (eq (current-column) 0) 0
    (let ((result (mod (current-column) arg)))
      (if (eq result 0) arg
        result))))

(defun backspace-some (arg)
  "Deletes some backspaces, ARG unused."
  (interactive "*P")
  (if (use-region-p) (backward-delete-char-untabify 1)
    (let ((here (point)))
      (if (eq 0 (skip-chars-backward " " (- (point) (get-deletion-count 4))))
          (backward-delete-char-untabify 1)
        (delete-region (point) here)))))

(defun azbyn/backspace ()
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backspace-some)))
;;(setq-default indent-tabs-mode t)

(define-key prog-mode-map (kbd "<backspace>") 'azbyn/backspace)

;; (add-hook 'prog-mode-hook (lambda ()
;;                             (interactive)
;;                             (local-set-key [backspace] 'azbyn/backspace)))
(setq backward-delete-char-untabify-method 'hungry)
;;(define-key 'multiple-cursors-mode-)
;; *** nicer delete word
(global-set-key (kbd "<C-backspace>") 'azbyn/kill-word-backward)
(global-set-key (kbd "<C-M-backspace>") 'azbyn/kill-subword-backward)
(global-set-key (kbd "<M-backspace>") 'azbyn/kill-subword-backward)
;; ** transpose args
(defun my-c-transpose-args--forward-to-argsep ()
  "Move to the end of the current c function argument.
  Returns point."
  (interactive)
  (while (progn
           (comment-forward most-positive-fixnum)
           (looking-at "[^,)]"))
    (forward-sexp))
  (point))

(defun my-c-transpose-args--backward-to-argsep ()
  "Move to the beginning of the current c function argument.
  Returns point."
  (interactive)
  (let ((pt (point))
        cur)
    (up-list -1)
    (forward-char)
    (while (progn
             (setq cur (point))
             (> pt (my-c-transpose-args--forward-to-argsep)))
      (forward-char))
    (goto-char cur)))

(defun my-c-transpose-args--direction (is_forward)
  "Transpose two arguments of a c-function.
  The first arg is the one with point in it."
  (interactive)
  (let* ((pt-original (point)) ;; only different to pt when not 'is_forward'
         (pt (progn
               (when (not is_forward)
                 (goto-char (- (my-c-transpose-args--backward-to-argsep) 1))
                 (unless (looking-at ",")
                   (goto-char pt-original)
                   (user-error "Argument separator not found")))
               (point)))
         (b (my-c-transpose-args--backward-to-argsep))
         (sep (progn
                (goto-char pt)
                (my-c-transpose-args--forward-to-argsep)))
         (e (progn
              (unless (looking-at ",")
                (goto-char pt-original)
                (user-error "Argument separator not found"))
              (forward-char)
              (my-c-transpose-args--forward-to-argsep)))
         (ws-first (buffer-substring-no-properties
                    (goto-char b)
                    (progn
                      (skip-chars-forward "[[:space:]\n]")
                      (point))))
         (first (buffer-substring-no-properties (point) sep))
         (ws-second (buffer-substring-no-properties
                     (goto-char (1+ sep))
                     (progn
                       (skip-chars-forward "[[:space:]\n]")
                       (point))))
         (second (buffer-substring-no-properties (point) e)))

    (delete-region b e)
    (insert ws-first second "," ws-second first)

    ;; Correct the cursor location to be on the same character.
    (if is_forward
        (goto-char
         (+
          ;; word start.
          (- (point) (length first))
          ;; Apply initial offset within the word.
          (- pt b (length ws-first))))
      (goto-char
       (+
        b (length ws-first)
        ;; Apply initial offset within the word.
        (- pt-original (+ pt 1 (length ws-second))))))))

(defun my-c-transpose-args-forward ()
  (interactive)
  (my-c-transpose-args--direction t))
(defun my-c-transpose-args-backward ()
  (interactive)
  (my-c-transpose-args--direction nil))
;; * local-defun
;;from  https://www.emacswiki.org/emacs/BufferLocalCommand
;; (defvar-local azbyn//local-obarray (make-vector 8 0))
(defvar azbyn//local-obarray (make-vector 8 0))
;; (defvar-local azbyn//local-obarray nil)

(defmacro azbyn//local-defun-impl (name arglist &optional docstring &rest body)
  ;; (unless azbyn/local-obarray
  ;;   (setq-local azbyn/local-obarray (make-vector 8 0)))

  (let* ((newname (intern (concat (symbol-name name) "-impl") azbyn//local-obarray)))
    `(defun ,newname ,arglist ,docstring ,@body)))


(defmacro azbyn/local-defun (name arglist &optional docstring &rest body)
  "A macro to define a buffer-local function. Its argument list is the same as the `defun' macro.
        Also defines a function called `name' that calls the buffer-local function
        NOTE in `azbyn//local-obarray' functions are stored with -impl at the end"
  ;; nil
  (declare (debug
            ;;from cl-macs (in cl-defun )
            ;; Same as iter-defun but use cl-lambda-list.
            (&define [&or name ("setf" :name setf name)]
                     cl-lambda-list
                     cl-declarations-or-string
                     [&optional ("interactive" interactive)]
                     def-body))
           (doc-string 3)
           (indent defun))
  ;; (unless azbyn/local-obarray
  ;;   (setq-local azbyn/local-obarray (make-vector 8 0)))

  `(progn
     (azbyn//local-defun-impl ,name ,arglist ,docstring ,body)
     (make-variable-buffer-local 'azbyn//local-obarray)
     )
  )

(defmacro azbyn/local-defun-default (name arglist &optional docstring &rest body)
  "Define a function that can be overridden with `azbyn/local-defun'.
        Usage is the same as `defun'"
  (declare (debug 
            ;; Same as iter-defun but use cl-lambda-list.
            (&define [&or name ("setf" :name setf name)]
                     cl-lambda-list
                     cl-declarations-or-string
                     [&optional ("interactive" interactive)]
                     def-body))
           (doc-string 3)
           (indent defun))

  ;; `(progn
  ;;    (azbyn//local-defun-impl ,name ,arglist ,docstring ,body)
  ;;    ;; (azbyn/local-defun ,name ,arglist ,docstring ,@body)
  ;;    (defun ,name ,arglist ,docstring
  ;;           (azbyn/call-local-defun ,(symbol-name name) ,@arglist)
  ;;           ))
  (let ((newname (intern (concat (symbol-name name) "-impl")
                         azbyn//local-obarray)
                 ))
    `(progn
       
       ;; (defun c0 () 3)
       (defun ,name ,arglist
         (funcall (intern ,(symbol-name newname) azbyn//local-obarray) ,@arglist))
       (defun ,newname ,arglist
         ,docstring ,@body)
       )
    )

  )


;; HACK not complete, only works for interactive functions
(defmacro azbyn/local-defalias (name value)
  `(azbyn/local-defun ,name ()
     (interactive) (call-interactively ,value))
  )
;; (unless azbyn/local-obarray
;;   (setq-local azbyn/local-obarray (make-vector 8 0)))

;; (let* ((newname (intern (symbol-name name) azbyn/local-obarray)))
;;   `(defalias ,newname ,value)))

(defun azbyn/call-local-defun (name &rest args)
  "Call a function defined with `azbyn/local-defun'"
  (let ((newname (intern;-soft
                  (concat name "-impl")
                  azbyn//local-obarray)))
    ;; (if (called-interactively-p newname)
    ;;     (call-interactively newname)
    (funcall newname args)))

;; 

;; (defun azbyn//init-obarray ()
;;   (setq-local azbyn//local-obarray azbyn//local-obarray-default))
;; (dolist (hook '(prog-mode-hook text-mode-hook fundamental-mode))
;;   (add-hook hook #'azbyn//init-obarray))
;; * compile TODO
;; ** project finding functions
;; *** misc
(defun azbyn/expand-name (path &optional current-dir)
  (expand-file-name (or (if (file-name-absolute-p path) path)
                        (let ((r-path path))
                          (setq r-path (substitute-in-file-name r-path))
                          (setq r-path (expand-file-name r-path current-dir))
                          r-path))))
;; (defun azbyn/updir (path)
;;   (let ((r-path (azbyn/expand-name path)))
;;     (if (and (> (length r-path) 0)
;;              (equal (substring r-path -1) "/"))
;;         (setq r-path (substring r-path 0 -1)))
;;     (if (eq (length r-path) 0)
;;         (setq r-path "/"))
;;     (directory-file-name
;;      (file-name-directory r-path))))

;; (require 'seq)
;; (defun azbyn/project-dir (path &optional pattern)
;;   "Find the first directory with a file that matches the pattern"
;;   (unless pattern (setq pattern "Makefile"))
;;   (if (or (not path) (member path '("/" "/home/azbyn/Projects" "/home/azbyn")))
;;       nil
;;     (if (seq-contains-p (directory-files path) pattern
;;                         (lambda (f _) (string-match-p pattern f)))
;;         ;;(member "Makefile" (directory-files path))
;;         path
;;       (azbyn/project-dir (azbyn/updir path) pattern))))
(defun azbyn/updir (path)
  "Returns the parent directory of =path=. For \"/\" it returns nil."
  (if (equal path "/")
      nil
    (expand-file-name ".." path)))

(require 'seq)
(defun azbyn/project-dir (path &optional pattern)
  "Find the first parent directory with a file that matches the pattern.
     Might or might not end in an infinite loop on /that non-free operating system/.
     (press C-g if that's the case)."
  (unless pattern (setq pattern "Makefile"))
  (if (not path)
      nil
    (if (seq-contains-p (directory-files path) pattern
                        (lambda (f _) (string-match-p pattern f)))
        path
      (azbyn/project-dir (azbyn/updir path) pattern))))
;; *** find root
(defun azbyn/find-root (npath)
  (if npath
      (if (file-directory-p npath)
          npath (azbyn/updir npath))
    nil))
;; *** ffip
(add-to-list 'ffip-prune-patterns "*/.mypy_cache")
(defun azbyn/ffip ()
  (interactive)
  ;; find-file-in-project doesn't really work for directories with a
  ;; lot of files
  (if (member (azbyn/find-root (buffer-file-name))
              '(nil "/" "/home/azbyn/Projects" "/home/azbyn"))
      (ivy-switch-buffer)
    (progn
      (find-file-in-project)
      ;;(insert-char ?/)
      )))
;; ** the function
;; these may be "overridden" in a sense by other modes
;; like in latex it would be useful to just run "pdflatex whateverfile.tex"
(defun azbyn/compile-project-command (path)
  "create a compile command depending on the directory"
  (cond ((member path '("/" "/home/azbyn/Projects" "/home/azbyn")) nil)
        ;; ((member ".dub" (directory-files path))
         ;; (message "dub build --root '%s'" path)) ; (directory-files path)))
        ((member "Makefile" (directory-files path))
         (concat "make -C '" path "'"))
        (t (azbyn/compile-project-command (azbyn/updir path)))))


;; (defvar azbyn/make-file-function 'compile)
(azbyn/local-defun-default azbyn/make-file ()
  "Compile the current file if no Makefile was found"
  (interactive)
  (message "azbyn/make-file not defined for current mode"))

;; (defun azbyn/run-make-file ()
;;   "Compile the current file if no makefile was found"
;;   (interactive)
;;   (azbyn/call-local-defun 'azbyn/make-file))

(defun azbyn/make-thing ()
  "If there's a makefile run that, else run `azbyn/make-file'"
  (interactive)
  (save-buffer)
  (let ((cc (azbyn/compile-project-command
             (azbyn/find-root (buffer-file-name)))))
    (if cc
        (progn
          (message "compile: `%s` `%s`" cc (azbyn/find-root (buffer-file-name)))
          (compile cc))
      ;;(message "thing")
      (azbyn/make-file)
      )))

(azbyn/local-defun azbyn/make-thing ()
  "If there's a makefile run that, else run `azbyn/make-file'"
  (interactive)
  (save-buffer)
  (let ((cc (azbyn/compile-project-command
             (azbyn/find-root (buffer-file-name)))))
    (if cc
        (let ((default-directory (azbyn/find-root (buffer-file-name))))
          (compile cc))
      ;;(message "thing")
      (azbyn/make-file)
      )))
;; (defun azbyn/run-make-thing ()
;;   "If there's a makefile run that, else run azbyn/make-file"
;;   (interactive)
;;   (azbyn/call-local-defun 'azbyn/make-thing))

;; (macroexpand '(azbyn/mode-local-defun compile () (print 1)))

;; (make-variable-buffer-local 'azbyn/make-file-function)
;; (defvar azbyn/make-thing-function
;;   (lambda ()
;;     (interactive)
;; ))
;; (make-variable-buffer-local 'azbyn/make-thing)


;; ** helper
(require 'cl-lib)

(cl-defun azbyn/compile-file-with (program &optional (format-str "%s \"%s\""))
  "Runs '<program> \"<buffer-file-name>\"'.
     Format is '%s \"%s\"' by default
     "
  (compile (format format-str program (buffer-file-name))))
;; * Keep the undo tree even after closing emacs
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-limit 643200)
  (setq undo-outer-limit 1857600)
  (setq undo-strong-limit 2286400)
  (setq undo-tree-mode-lighter " UN")
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (add-hook 'undo-tree-visualizer-mode-hook (lambda ()
                                              (undo-tree-visualizer-selection-mode)
                                              (setq display-line-numbers nil)))
  :config
  (global-undo-tree-mode 1))
(defun azbyn/nuke-undo-tree ()
  (interactive)
  (setq buffer-undo-tree nil))


;; * ivy and counsel mode
;; ** smex for showing recent commands
(use-package smex
  :ensure t)
;; ** actual install
(use-package counsel
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-height 12)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  ;; (defun counsel-M-x-no-init()
  ;; (interactive)
  ;; (counsel-M-x ""))
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;;(global-set-key (kbd "C-c g") 'counsel-git)
  ;;(global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;;(global-set-key (kbd "C-c k") 'counsel-ag)
  ;;(global-set-key (kbd "C-x l") 'counsel-locate)
  ;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
  )
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
;; * swiper
(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))
;; ** search previous thing
(defun azbyn/swiper-search-previous ()
  (interactive)
  (swiper isearch-string))
(global-set-key (kbd "C-S-s") 'azbyn/swiper-search-previous)
(global-set-key (kbd "C-M-s") 'azbyn/swiper-search-previous)

;; * evil mode
;; Don't really use evil mode the conventional way - I just use it for some nice vim functons like =da{= and other things.

(use-package evil
  :ensure t)
;;(unless (package-installed-p 'evil)
;; (package-install 'evil))

;; Enable Evil
;;(require 'evil)

;; ** emacs state by default
(setq-default evil-default-state 'emacs)

;; I don't really want to use vim mode ever, so i bind the switch to something hard to reach.
(setq-default evil-toggle-key "H-M-C-s-e")

;; ** disable some keybindings
(define-key evil-visual-state-map (kbd "C-w") nil)
(define-key evil-motion-state-map (kbd "C-w") nil)
(define-key evil-emacs-state-map  (kbd "C-z") nil)
(define-key evil-motion-state-map (kbd "C-z") nil)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "C-o") nil)
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-motion-state-map (kbd "C-i") nil)
(define-key evil-motion-state-map (kbd "C-u") nil)
(define-key evil-motion-state-map (kbd "C-d") nil)
;;don't start eshell and others in insert mode
(setq-default evil-insert-state-modes nil)

;; ** a nice cursor
(setq-default evil-emacs-state-cursor (list base16-col-yellow 'box))
(blink-cursor-mode 0)

;; ** finaly enable evil
(evil-mode 1)
;; * company mode
;; also use =C-n=, =C-p= for movement
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2) ;; so we can enter // comments
  (setq company-selection-wrap-around t)
  (setq company-require-match nil)
  ;;(company-tng-configure-default)
  )
(with-eval-after-load 'company
  (setq company-backends (cons 'company-files
                               (remove 'company-files company-backends)))
  (define-key company-active-map [escape] 'company-abort)
  (define-key company-active-map (kbd "C-h") nil)
  ;;(define-key company-active-map (kbd "M-n") nil)
  ;;(define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map [C-j] 'company-select-next)
  (define-key company-active-map [C-i] 'company-select-previous)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
                                        ;(define-key company-active-map (kbd "SPC") #'company-abort)
  (define-key company-active-map (kbd "SPC") nil)
  )
;; * smartrep
(use-package smartrep
  :ensure t
  :config
  (smartrep-define-key
      global-map "C-x"
    '(("<left>" .  previous-buffer)
      ("<right>" . next-buffer))))
;; * winner mode
(winner-mode 1)
(smartrep-define-key
    winner-mode-map "C-c"
  '(("<left>" . winner-undo)
    ("<right>" . winner-redo)))
;; * server
;; (unless azbyn/windows-mode
(load "server")
(unless (server-running-p) (server-start))
;; )

;; * quail stuff
(require 'russian-transl)
(require 'azbyn-tex)
(require 'old-romanian-quail)
;; (setq-default default-input-method "azbyn-TeX")
(setq-default default-input-method "old-romanian")

;; * programming languages config
;; ** common
(when (require 'so-long nil :noerror)
  (global-so-long-mode 1))

;;(global-set-key (kbd "M-m") 'yas-expand)
;;(setq compilation-scroll-output t)
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun prog-hook ()
  (setq tab-width 4)
  (hs-minor-mode)
  (yas-minor-mode))
(defun azbyn/company-flycheck ()
  (interactive)
  (flycheck-mode)
  (company-mode))

(add-hook 'prog-mode-hook 'prog-hook)
;; ** conf mode

(defun azbyn/conf-hook ()
  (highlight-numbers-mode)
  (display-line-numbers-mode))
(add-hook 'conf-unix-mode-hook 'azbyn/conf-hook)

;; ** comint (*inferior <language>*)
;; (define-key comint-mode-map (kbd "M-I") 'comint-previous-input)
;; TODO separate file .el
(defun azbyn-comint-hook ()
  (local-set-key (kbd "M-r") 'comint-previous-input)
  (local-set-key (kbd "M-q") 'comint-next-input)
  )
(add-hook 'comint-mode-hook 'azbyn-comint-hook)

;; (defun azbyn/get-comint-process ()
;;   ;;TODO overload
;;   ;;TODO create if it doesn't exist
;;   ;;TODO popup if it doesn't exist
;;   (inferior-haskell-process))
(defun azbyn/get-interactive-buffer-name ()
  "*haskell*")
;; (haskell-process))

;;modified from python.el
;; (setq process (inferior-haskell-process))
;; TODO use comint-send-region
(defun azbyn/send-comint-string (string)
  (let ((buffer-name (azbyn/get-interactive-buffer-name)))
    (with-current-buffer buffer-name
      (insert string)
      (comint-send-input nil t)
      ;; (let ((process  (azbyn/get-comint-process)))
      ;;   ;;TODO multiline
      ;;   (comint-send-string process string)
      ;;   (comint-send-string process "\n")
      ;; (if (string-match ".\n+." string)   ;Multiline.
      ;;     ;; (let* ((temp-file-name (python-shell--save-temp-file string))
      ;;     ;;        (file-name (or (buffer-file-name) temp-file-name)))
      ;;     ;;   (python-shell-send-file file-name process temp-file-name t))
      ;;     ;;TODO does this work? prolly not
      ;;     (comint-send-string process string)
      ;;   (comint-send-string process string)
      ;;   (when (or (not (string-match "\n\\'" string))
      ;;             (string-match "\n[ \t].*\n?\\'" string))
      ;;     (comint-send-string process "\n")))

      )
    (display-buffer buffer-name)
    ))

;;TODO
;; (defun azbyn/send-region )

;; (define-key comint-mode-map (kbd "M-J") 'comint-next-input)
;; ** lsp
(unless azbyn/windows-mode
  (use-package lsp-mode
    :ensure t
    :init
    (setq lsp-prefer-flymake nil)
    :demand t
    :after jmi-init-platform-paths)
  (defun azbyn/lsp-hook ()
    (setq azbyn/goto-definition-function 'lsp-find-definition)
    ;; (setq azbyn/goto-definition-function 'lsp-goto-implementation)
    (define-key lsp-mode-map (kbd "<C-return>") 'lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "C-M-g") 'lsp-goto-type-definition);; lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "C-M-b") 'lsp-goto-implementation);; lsp-execute-code-action)
    )
  (add-hook 'lsp-mode-hook 'azbyn/lsp-hook)

  (setq lsp-modeline-code-actions-segments '())
  (setq lsp-modeline-diagnostics-enable nil)
  (use-package lsp-ui
    :ensure t
    :config
    (setq lsp-ui-doc-enable nil
          lsp-ui-sideline-enable nil
          ;; lsp-headerline-breadcrumb-enable t ;eh, keep the top thing
          lsp-ui-flycheck-enable t)
    :after lsp-mode)
  ;; auto formatting messes stuff up. best disable it
  (add-hook 'lsp--managed-mode-hook (lambda nil (interactive) (remove-hook 'post-self-insert-hook 'lsp--on-self-insert t)))
  (use-package all-the-icons
    :ensure t)

  (use-package dap-mode
    :ensure t
    :config
    (dap-mode t)
    (dap-ui-mode t)))
;; ** clang-format
(use-package clang-format
  :ensure t)
(setq-default clang-format-fallback-style "llvm")
(defun azbyn/clang-format-region-or-buffer ()
  (interactive)
  (if mark-active
      (call-interactively 'clang-format-region)
    (clang-format-buffer)))

(bind-key "<C-M-tab>" 'azbyn/clang-format-region-or-buffer)
;; ** c-style

(diminish 'company-dcd-mode)
(diminish 'company-dcd-mode)
(c-add-style "my-style"
             '("stroustrup"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)
               (c-offsets-alist
                (inlambda . 0) ; no extra indent for lambda
                ;; (member-init-intro . '++)
                (member-init-intro . 8)
                (innamespace . -))))

(push '(other . "my-style") c-default-style)
;; ** elisp
(define-key emacs-lisp-mode-map (kbd "M-q") 'backward-sexp)
(define-key emacs-lisp-mode-map (kbd "M-r") 'forward-sexp)
;;(add-hook 'emacs-lisp-mode-hook 'semantic-mode)
;; (setq lisp-indent-function 'common-lisp-indent-function)

(defun azbyn/elisp-hook ()
  (flycheck-mode)
  (company-mode)
  (azbyn/local-defun azbyn/make-thing ()
    (interactive)
    (eval-buffer)
    (setq tab-width 8)
    (message "Evaluated buffer"))
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )
(add-hook 'emacs-lisp-mode-hook #'azbyn/elisp-hook)
;; ** latex
(unless azbyn/windows-mode
  (use-package auctex
    :defer t
    :ensure t)
  (use-package company-auctex
    :defer t
    :ensure t)
  (defun azbyn/tex-hook()
    (company-mode)
    (prettify-symbols-mode -1)
    (flyspell-mode)
    (flycheck-mode))
  (add-hook 'TeX-mode-hook 'azbyn/tex-hook)
  )

;; ** ptry
(unless azbyn/windows-mode
  (require 'poetry-mode)
  (add-hook 'poetry-mode-hook 'display-line-numbers-mode))
;; ** lambda
(unless azbyn/windows-mode
  (require 'lambda-mode))
;; ** dlang
(unless azbyn/windows-mode
  (use-package d-mode
    :ensure t)

  (use-package flycheck-dmd-dub
    :ensure t)
  (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables)
  (use-package company-dcd
    :ensure t
    :diminish abbrev-mode
    :diminish company-dcd-mode)

  (add-hook 'd-mode-hook 'company-dcd-mode)
  (add-hook 'd-mode-hook 'flycheck-mode)
  
  ;; (setq flycheck-dmd-include-path
  ;;       '("~/.dub/packages/gtk-d-3.9.0/gtk-d/generated/gtkd"
  ;;         "~/.dub/packages/matplotlib-d-0.1.7/matplotlib-d/source/"))
  ;; (setq flycheck-dmd-args '("-J" "~/.dub/packages/matplotlib-d-0.1.7/matplotlib-d/views/pyplot_functions.txt"))

  (setq flycheck-dmd-args '())
  (add-hook 'd-mode-hook (lambda ()
                           (setq azbyn/goto-definition-function
                                 'company-dcd-goto-definition)
                           (setq azbyn/search-symbol-function
                                 'company-dcd-ivy-search-symbol))))
;;add ddoc (ie `C-c ?' now) to some keybinding?
;; ** rust
(unless azbyn/windows-mode
  (use-package rust-mode
    :ensure t)
  (use-package company-racer
    :ensure t)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer)))
;; ** lua
(unless azbyn/windows-mode
  (use-package lua-mode
    :ensure t
    :defer 1
    )
  ;; (use-package lua-mode
  ;;   :ensure t)
  ;; (setq lua-indent-size 4)
  (add-hook 'lua-mode-hook 'company-mode)
  (add-hook 'lua-mode-hook 'flycheck-mode))
;; ** fish
(unless azbyn/windows-mode
  (use-package fish-mode
    :ensure t))
;; ** sh
(add-hook 'sh-mode-hook 'azbyn/company-flycheck)
;; ** xmodmap
(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")
;; ** nasm
(unless azbyn/windows-mode
  (use-package nasm-mode
    :ensure t
    :config
    (setq nasm-basic-offset 4)
    (define-key nasm-mode-map (kbd ";") nil)
    (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
    (add-hook 'nasm-mode-hook 'company-mode)
    ))

;; ** python
(use-package company-jedi
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-jedi))
(use-package jedi
  :ensure t)

(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'jedi-mode)
;;(add-hook 'python-mode-hook 'jedi:setup)
(when azbyn/windows-mode
  (setq python-scripts-path (shell-command-to-string "py -c \"from distutils.sysconfig import get_python_lib; print(get_python_lib().replace(r'Lib\\site-packages', 'Scripts\\\\'), end='')\""))
  ;;(setq python-scripts-path "C:/Users/azbyn/AppData/Local/Programs/Python/Python37/Scripts/")
  (setq python-environment-virtualenv (list (concat python-scripts-path "virtualenv.exe")))
  (setq python-shell-interpreter (concat python-scripts-path "ipython.exe"))

  (setq flycheck-python-pycompile-executable "C:/LegacyApp/Python36/python.exe")
  ;; you might have to redefine flycheck-temp-files-writable-p to return only t
  (setq flycheck-python-pylint-executable (concat python-scripts-path "pylint.exe"))
  (setq flycheck-python-mypy-executable (concat python-scripts-path "mypy.exe"))
  )

(use-package elpy
  :ensure t
  :diminish elpy-mode
  :init
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       ;; elpy-module-flymake
                       ;;elpy-module-highlight-indentation
                       ;; elpy-module-pyvenv
                       elpy-module-yasnippet
                       ;;elpy-module-django
                       ))
  (elpy-enable)
  ;;    (setq elpy-rpc-backend "jedi")
  ;;(add-hook 'python-mode-hook 'company-mode)
  )


(defun azbyn/python-eval-stmt()
  (interactive)
  (if mark-active
      (elpy-shell-send-region-or-buffer)
    (elpy-shell-send-statement)))

(define-key elpy-mode-map (kbd "<C-return>") 'azbyn/python-eval-stmt)
(define-key elpy-mode-map (kbd "<M-return>") 'elpy-shell-send-defun)
(define-key elpy-mode-map (kbd "<M-S-return>") 'elpy-shell-send-defclass)
(define-key elpy-mode-map (kbd "<C-S-return>") 'elpy-shell-send-defclass)


;;(setq python-shell-interpreter "jupyter"
;;    python-shell-interpreter-args "console --simple-prompt"
;;    python-shell-prompt-detect-failure-warning nil)
;;(add-to-list 'python-shell-completion-native-disabled-interpreters
;;           "jupyter")
(unless azbyn/windows-mode
  (setq python-shell-interpreter "ipython"))
(setq python-shell-interpreter-args "-i --simple-prompt")

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)
(diminish 'compilation-shell-minor-mode)

(setq-default python-indent-offset 4)
(defun azbyn/py-run-current-file ()
  (interactive)
  (azbyn/compile-file-with "py"))

(defun azbyn/pylint-current-file ()
  (interactive)
  (azbyn/compile-file-with "py -m pylint"))

(defun azbyn/mypy-current-file ()
  (interactive)
  (azbyn/compile-file-with "py -m mypy"))


(defun azbyn/python-hook ()
  ;; (local-set-key (kbd "M-c") (lambda ()(interactive)
  ;;                              ))
  (local-set-key (kbd "M-c") 'azbyn/py-run-current-file)
  (local-set-key (kbd "M-v") 'azbyn/pylint-current-file)
  (local-set-key (kbd "M-V") 'azbyn/mypy-current-file)

  (if azbyn/windows-mode
      (setq-local flycheck-disabled-checkers '(python-pylint python-mypy))
    (setq-local flycheck-disabled-checkers '(python-pylint python-mypy))
    )

  (azbyn/local-defalias azbyn/make-file 'azbyn/py-run-current-file)
  ;; (advice-add 'azbyn/make-file :override 'azbyn/py-run-current-file)
  ;; (advice-add 'azbyn/make-file :override 'elpy-shell-send-buffer)
  ;; (setq azbyn/make-file-function 'elpy-shell-send-buffer)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (setq azbyn/goto-definition-function 'jedi:goto-definition)
  (setq azbyn/search-symbol-function 'elpy-rgrep-symbol)
  (setq azbyn/repl-name "*Python*"))

(add-hook 'python-mode-hook 'azbyn/python-hook)
(setq auto-mode-alist
      (cons '("\\.pylintrc\\'" . conf-unix-mode) auto-mode-alist))

;; ** ipython
(unless azbyn/windows-mode
  (use-package ob-ipython
    :ensure t)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-image-actual-width 500))
;; ** dumb-c-mode
(defun azbyn/dumb-c-mode ()
  (let ((c-mode-hook nil))
    (c-mode)))

;;this doesn't really work for some reason
(define-derived-mode dumb-c-mode
  azbyn/dumb-c-mode "Dumb-c"
  "C without the ide features."
  ;; (setq-local case-fold-search nil)
  )
;; ** canoe
(setq auto-mode-alist
      (append '(("\\.can$" . dumb-c-mode)
                ("\\.cin$" . dumb-c-mode)) auto-mode-alist))

;; ** c++
(use-package cmake-mode
  :ensure t)

(unless azbyn/windows-mode
  (use-package irony
    :diminish irony-mode
    :ensure t)
  (use-package irony-eldoc
    :ensure t)
  ;;i don't like autopairing  for this
  (sp-local-pair '(c-mode c++-mode) "/*" nil :actions :rem)

  ;;(add-hook 'c++-mode-hook 'semantic-mode)
  ;;(add-hook 'c-mode-hook 'semantic-mode)

  (add-hook 'c-mode-hook 'azbyn/remove-sp)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)

  (add-hook 'c-mode-hook 'irony-mode)
  ;; (add-hook 'c++-mode-hook 'company-mode)
  ;; (add-hook 'c++-mode-hook 'flycheck-mode)

  (defun azbyn/c++-hook ()
    (company-mode)
    (flycheck-mode)
    (irony-mode)
    (irony-eldoc)

    (setq flycheck-clang-language-standard "c++20")
    (when (fboundp 'platformio-conditionally-enable)
      ;;if that's defined we prolly have the whole platformio stuff
      (platformio-conditionally-enable)

      ;;idk why this doesn't work
      (when (projectile-verify-file "platformio.ini")
        (azbyn/platformio-setup)
        )
      ))
  (define-key c++-mode-map (kbd "<") nil)
  (define-key c++-mode-map (kbd ">") nil)
  
  ;; (define-key c++-mode-map (kbd "C-c C-k") nil)
  
  (defun azbyn/platformio-setup ()
    (interactive)
    ;; (setq azbyn/make-file-function 'platformio-upload)
    (local-set-key (kbd "M-c") 'platformio-upload)
    (setq flycheck-clang-include-path
          (list
           (expand-file-name "~/.platformio/packages/toolchain-atmelavr/avr/include/")
           (expand-file-name "~/.platformio/packages/framework-arduino-avr/variants/atmega328pb/")
           "/usr/share/arduino/hardware/archlinux-arduino/avr/cores/arduino/"))
    (setq flycheck-clang-args '("-nostdinc++" "--target=avr"
                                "-I/usr/share/arduino/hardware/archlinux-arduino/avr/cores/arduino/"
                                )))
  (defun read-lines (filePath)
    "Return a list of lines of a file at filePath."
    (with-temp-buffer
      (insert-file-contents filePath)
      (split-string (buffer-string) "\n" t)))


  (defun azbyn/load-clang-complete ()
    (interactive)
    (setq flycheck-clang-args (read-lines "../.clang_complete"))
    (message "loaded .clang_complete"))

  (defun azbyn/psp-setup ()
    (interactive)
    (setq flycheck-clang-args '("-I/usr/local/pspdev/psp/sdk/include"
                                "-I/usr/local/pspdev/psp/include"
                                "-D_PSP_FW_VERSION=150"
                                )))

  (add-hook 'c++-mode-hook 'azbyn/c++-hook)

  ;;i don't like having /* autocompleted
                                        ;(define-key c++-mode-map (kbd "*") nil)


  (with-eval-after-load "flycheck"
    (setq flycheck-clang-warnings `(,@flycheck-clang-warnings
                                    "no-pragma-once-outside-header")))
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  '(c/c++-cppcheck c/c++-gcc)))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))
;; ** java
(unless azbyn/windows-mode
  (defun azbyn/java-hook ()
    (setq-local tab-width 4
                c-basic-offset 4)
    ;; (toggle-truncate-lines 1)
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4)
    (company-mode)
    (flycheck-mode)
    (lsp)
    (lsp-managed-mode -1))
  (add-hook 'java-mode-hook 'azbyn/java-hook)

  (use-package lsp-java
    :ensure t

    :config
    ;; Enable dap-java
    (require 'dap-java)

    ;; Support Lombok in our projects, among other things
    ;; (setq lsp-java-vmargs
    ;;       (list "-noverify"
    ;;             "-Xmx2G"
    ;;             "-XX:+UseG1GC"
    ;;             "-XX:+UseStringDeduplication"
    ;;             (concat "-javaagent:" jmi/lombok-jar)
    ;;             (concat "-Xbootclasspath/a:" jmi/lombok-jar))
    ;;       lsp-file-watch-ignored
    ;;       '(".idea" ".ensime_cache" ".eunit" "node_modules"
    ;;         ".git" ".hg" ".fslckout" "_FOSSIL_"
    ;;         ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
    ;;         "build")

    ;;       lsp-java-import-order '["" "java" "javax" "#"]
    ;;       ;; Don't organize imports on save
    ;;       lsp-java-save-action-organize-imports nil

    ;;       ;; Formatter profile
    ;;       lsp-java-format-settings-url
    ;;       (concat "file://" jmi/java-format-settings-file))


    :demand t
    :after (lsp lsp-mode dap-mode jmi-init-platform-paths)))
;; ** haskell
(unless azbyn/windows-mode
  (use-package haskell-mode
    :ensure t)
  (setq haskell-interactive-popup-errors nil)
  (setq flycheck-ghc-args '("-dynamic"))
  (defun azbyn//haskell-send-str (str)
    ;; (save-excursion
    (with-current-buffer "*haskell*"
      (haskell-interactive-mode-eval-result (haskell-session) (format "\n%s\n" str))
      (haskell-interactive-mode-run-expr str)
      (goto-char (point-max))));)
  (defun azbyn/haskell-compile-and-run ()
    (interactive)
    ;; (let ((buffer (buffer-name)))
    (azbyn//haskell-send-str (format ":load %s" (buffer-name)));;:main
    (azbyn//haskell-send-str ":main")
    ;; ()

    ;; )
    ;; (haskell-interactive-bring)
    ;; (goto-char (point-max))

    ;; (haskell-interactive-mode-return)
    )
  (defun azbyn/haskell-compile ()
    (interactive)
    (save-buffer)
    ;; (let ((buffer (buffer-name)))
    (azbyn//haskell-send-str (format ":load %s" (buffer-name))))


  (defun azbyn/haskell-send ()
    (interactive)
    (azbyn//haskell-send-str
     (if mark-active
         (buffer-substring-no-properties (region-beginning) (region-end))
       (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
  (defun azbyn/haskell-hook ()
    ;;(interactive-haskell-mode)
    (setq-local flycheck-disabled-checkers '(haskell-stack-ghc))
    ;; (local-set-key (kbd "M-c") 'azbyn/haskell-compile)

    (company-mode)
    (flycheck-mode))
  ;; TODO proper eval-stmt
  (define-key haskell-mode-map (kbd "M-c") 'azbyn/haskell-compile);-and-run)
  (define-key haskell-mode-map (kbd "C-c C-c") 'azbyn/haskell-compile)
  (define-key haskell-mode-map (kbd "<C-return>") 'azbyn/haskell-send);; 'haskell-interactive-bring)

  (add-hook 'haskell-mode-hook 'azbyn/haskell-hook)
  )
;; ** arduino
(unless azbyn/windows-mode
  (use-package platformio-mode
    :ensure t)
  (use-package arduino-mode
    :ensure t)
  (use-package company-arduino
    :ensure t)
  (add-hook 'arduino-mode-hook 'company-mode)
  (add-hook 'arduino-mode-hook 'flycheck-mode)
  (add-hook 'arduino-mode-home 'azbyn/key-bindings)
  (add-hook 'arduino-mode-hook
            (lambda ()
              ;; (setq azbyn/make-file-function 'arduino-upload)
              (local-set-key (kbd "M-c") 'arduino-upload)
              (setq c-basic-offset 4)
              (setq tab-width 4)
              )))

;; ** R
(unless azbyn/windows-mode
  (use-package ess
    :ensure t)
  (defun azbyn/ess-eval-stmt ()
    (interactive)
    (if mark-active
        (call-interactively 'ess-eval-region)
      (ess-eval-line t))
    (ess-eval-region-or-function-or-paragraph t))

  (defun azbyn/ess-eval-func ()
    (interactive)
    (ess-eval-region-or-function-or-paragraph t))

  (defun azbyn/ess-hook ()
    (local-set-key (kbd "<C-return>") 'ess-eval-region-or-line-visibly-and-step);; azbyn/ess-eval-stmt)
    (local-set-key (kbd "<M-return>") 'azbyn/ess-eval-func)
    (local-set-key (kbd "C-c C-c") 'ess-eval-buffer))
  (add-hook 'ess-mode-hook 'azbyn/ess-hook)
  )
;; ** octave
(unless azbyn/windows-mode
  (defun azbyn/octave-run-current-file ()
    (interactive)
    (azbyn/compile-file-with "octave"))
  (setq auto-mode-alist
        (cons '("\\.m$" . octave-mode) auto-mode-alist))
  (add-hook 'octave-mode-hook 'azbyn/octave-hook)
  (defun azbyn/octave-hook ()
    (company-mode)
    (flycheck-mode)

    (define-key octave-mode-map (kbd "<C-return>") 'octave-send-line)
    (define-key octave-mode-map (kbd "<M-return>") 'octave-send-defun)
    (define-key octave-mode-map (kbd "<M-S-return>") 'octave-send-block)
    (local-set-key (kbd "M-c") 'azbyn/octave-run-current-file)
    ;; (define-key octave-mode-map (kbd "M-c") 'octave-send-block)
    (define-key octave-mode-map (kbd "<C-S-return>") 'octave-send-block)
    (define-key octave-mode-map (kbd "C-c C-c") 'octave-send-buffer)
    ))
;; ** web
(use-package web-mode
  :ensure t)
(defun azbyn/web-hook ()
  (prettify-symbols-mode -1))
(add-hook 'web-mode-hook 'azbyn/web-hook)

;; ** php
(unless azbyn/windows-mode
  (use-package php-mode
    :ensure t)
  (defun azbyn/php-hook ()
                                        ;(flycheck-mode)
    (company-mode)
    (prettify-symbols-mode -1)
    (auto-revert-mode -1)
    )
  (add-hook 'php-mode-hook 'azbyn/php-hook))
;; ** groovy
(use-package groovy-mode
  :ensure t)
(setq auto-mode-alist
      (cons '("\\.groovy$" . groovy-mode) auto-mode-alist))
(defun azbyn/groovy-hook ()
  (setq tab-width 4))
(add-hook 'groovy-mode-hook 'azbyn/groovy-hook)
;; ** json
(use-package json-mode
  :ensure t)
;; ** yaml
(use-package yaml-mode
  :ensure t)

;; ** c#
;; having syntax highlighting is enough
(when azbyn/windows-mode
  (when (>= emacs-major-version 26)
    (use-package csharp-mode
      :ensure t)))

(unless azbyn/windows-mode
  (use-package csharp-mode
    :ensure t)
  (setq auto-mode-alist
        (cons '("\\.csproj$" . xml-mode) auto-mode-alist))



  ;; (defun call-dotnet (&rest args)
  ;;   (make-process :name "run-dotnet"
  ;;                 :buffer nil
  ;;                 :command (cons "dotnet" args)))
  ;; (setq lexical-binding t)
  (defun csharp/new-project ()
    (interactive)
    (let* ((parent-dir (read-directory-name "Parent Directory: "))
           (proj-name (read-string "Project Name: "))
           (template (read-string "Template: " "console"))
           (full-path (expand-file-name proj-name parent-dir)))

      (unless (and (file-directory-p full-path)
                   (not (y-or-n-p (format "Directory %s already exists. Continue? " full-path))))
        (message "Generating '%s'..." full-path)
        (lsp-async-start-process
         (lambda ()
           (message "oi")
           (message "oi '%s'" full-path)
           (let ((f (expand-file-name "Program.cs" full-path)))
             (if (file-exists-p f)
                 (progn
                   (message "Done.")
                   (find-file f))
               (message "Something went wrong :("))))
         (lambda (why)
           (message "Something went wrong: '%s'" why))
         "dotnet" "new" template "-o" full-path))))

  (defun csharp/run-project ()
    (interactive)
    (let ((dir (azbyn/project-dir (azbyn/find-root buffer-file-name) ".*\\.csproj$")))
      (if dir
          (compile (format "dotnet run -p '%s'" dir))
        (message "Not inside a C# project. (.csproj file not found)"))
      ))

  ;; var not being purple is annoying

  (c-lang-defconst c-type-modifier-kwds
    csharp '("readonly" "new" "var"))
  ;; (c-lang-defconst c-typeless-decl-kwds
  ;;          csharp '("var"))
  ;;        (c-lang-defconst c-other-decl-kwds
  ;;          csharp '("var"))
  ;;        ;;(c-lang-defconst c-type-modifier-kwds
  ;;        ;;  csharp '("readonly" "new" "var"))
  ;;        (c-lang-defconst c-primitive-type-kwds
  ;;          csharp '("bool" "byte" "sbyte" "char" "decimal" "double" "float" "int" "uint"
  ;;                   "long" "ulong" "short" "ushort" "void" "object" "string"))


  (c-lang-defconst c-other-decl-kwds
    csharp '("var"))
  (c-lang-defconst c-modifier-kwds
    csharp '("abstract" "default" "final" "native" "private" "protected"
             "public" "partial" "internal" "readonly" "static" "event" "transient"
             "volatile" "sealed" "ref" "out" "virtual" "implicit" "explicit"
             "fixed" "override" "params" "async" "await" "extern" "unsafe"
             "get" "set" "this" "const" "delegate"))

  (c-lang-defconst c-primitive-type-kwds
    csharp '("bool" "byte" "sbyte" "char" "decimal" "double" "float" "int" "uint"
             "long" "ulong" "short" "ushort" "void" "object" "string"))

  (define-key csharp-mode-map (kbd "M-c") 'csharp/run-project)
  (defun azbyn/csharp-hook ()
    (flycheck-mode)
    (company-mode)
    (lsp))
  (add-hook 'csharp-mode-hook 'azbyn/csharp-hook))
;; ** f#
(unless azbyn/windows-mode
  (use-package fsharp-mode
    :defer t
    :ensure t))
;; ** kotlin
(unless azbyn/windows-mode
  (defun azbyn/kotlin-hook ()
    ;; (setq-local tab-width 4
    ;;             c-basic-offset 4)
    ;; ;; (toggle-truncate-lines 1)
    ;; (setq-local tab-width 4)
    ;; (setq-local c-basic-offset 4)
    (company-mode)
    (flycheck-mode)
    ;; (lsp)
    ;; (lsp-managed-mode -1)
    )
  (use-package kotlin-mode
    :ensure t)
  (use-package flycheck-kotlin
    :ensure t)
  ;; (use-package kotlin-ls
  ;;   :ensure t)
  (add-hook 'kotlin-mode-hook 'azbyn/kotlin-hook)
  )
;; ** julia
(unless azbyn/windows-mode
  (use-package julia-mode
    :ensure t))
;; ** clojure
(unless azbyn/windows-mode
  (use-package clojure-mode
    :ensure t)
  (use-package cider
    :ensure t)
  (use-package parseedn
    :ensure t)
  (use-package parseclj
    :ensure t)
  (use-package clj-refactor
    :ensure t)

  (defun azbyn/eval-clojure-region-or-expr ()
    (interactive)
    (if mark-active
        (call-interactively 'cider-eval-region)
      (call-interactively 'cider-eval-last-sexp)))

  (defun azbyn/clojure-hook ()
    ;; (message "oi")
    (azbyn/local-defun azbyn/make-thing ()
      (interactive)
      (cider-ns-reload-all)
      (cider-switch-to-repl-buffer))
    (clj-refactor-mode)
    (local-set-key (kbd "M-c") 'cider-eval-buffer)
    (local-set-key (kbd "<C-return>") 'azbyn/eval-clojure-region-or-expr)
    (company-mode)
    (flycheck-mode))
  (add-hook 'clojure-mode-hook 'azbyn/clojure-hook)
  (defun azbyn/clojure-repl-hook ()
    (rainbow-delimiters-mode-enable))
  (add-hook 'cider-repl-mode-hook 'azbyn/clojure-repl-hook))

;; ** org
(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (C . t)
                             (octave . t)
                             (python . t)
                             ;;(ipython . t)
                             ))
(setq org-confirm-babel-evaluate nil)
(defun azbyn/org-hook ()
  ;; (message "oi")
  (azbyn/local-defun azbyn/make-thing ()
    (interactive)
    (local-set-key (kbd "M-c") 'org-latex-export-to-pdf)
    (org-latex-export-to-pdf))
  ;; (azbyn/local-defalias azbyn/make-thing 'org-latex-export-to-pdf)
  ;; (setq-local azbyn/make-thing-function )
  ;; (add-to-list 'org-latex-minted-langs '(ipython "python"))
  (company-mode)
  (hl-todo-mode))

(add-hook 'org-mode-hook 'azbyn/org-hook)


(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; * end
(provide 'config)
