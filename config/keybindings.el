;; * =C-c= and hyper bindings
;; I keep them here so i don't define the same key multiple times.
;; I define them for both `C-c' and hyper because i don't have my hyper key when
;; in a terminal


;; (require 'config)

(require 'quail)
(define-key quail-conversion-keymap (kbd "<delete>") nil)

(defun global-set-hyper-key (keyChar func)
  (global-set-key (kbd (concat "C-c " keyChar)) func)
  (global-set-key (kbd (concat "H-" keyChar)) func))
;; ** config related
  (global-set-hyper-key "r" 'config-reload)
  (global-set-hyper-key "e" 'config-visit)
  (global-set-hyper-key "E" 'config-visit-readonly)
;; ** show flycheck errors
  (global-set-hyper-key "F" 'flycheck-list-errors)
;; ** delete word
  (global-set-hyper-key "w" 'daedreth/kill-inner-word)
;; ** evil join
  (global-set-hyper-key "j" 'evil-join)
;; ** sort
  (global-set-hyper-key "s" 'sort-lines)
  (global-set-hyper-key "S" 'sort-fields)
;; ** quote
(global-set-hyper-key "q" 'quoted-insert)
;; ** delete window
  (global-set-hyper-key "Q" 'delete-window)
;; ** ffip
  (global-set-hyper-key "b" 'azbyn/ffip)
  (global-set-key (kbd "M-z") 'azbyn/ffip)
;; ** beginning of line
  (global-set-hyper-key "a" 'beginning-of-line)
;; ** kill this buffer
  (global-set-key (kbd "C-c k") 'kill-this-buffer)
;; ** exit minibuffer
  (global-set-hyper-key "g" 'exit-minibuffer)
;; * transpose
  (global-set-key (kbd "C-x T") 'transpose-chars)
  (global-set-key (kbd "C-x t") 'transpose-words)
;; * key maps
;; ** M-t
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode 1))
(defun azbyn/anzu-replace-regexp ()
  "no query for me"
  (interactive)
  (anzu--query-replace-common t
                              ;;:at-cursor t
                              ;;:thing anzu-replace-at-cursor-thing
                              :query nil))

(define-prefix-command 'meta-t-key-map)
(global-set-key (kbd "M-t") 'meta-t-key-map)
(global-set-key (kbd "M-t r") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-t C-t") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-t M-t") 'azbyn/anzu-replace-regexp)
(global-set-key (kbd "M-t q") 'anzu-query-replace)
(global-set-key (kbd "M-t c") 'anzu-replace-at-cursor-thing)

(global-set-key (kbd "M-t t") 'delete-trailing-whitespace)
;; ** M-g
;; *** movement
(global-set-key (kbd "M-g t") 'beginning-of-buffer)
(global-set-key (kbd "M-g b") 'end-of-buffer)
(global-set-key (kbd "M-g a") 'avy-goto-char)
;; *** dumb jump
(use-package dumb-jump
  :ensure t)

(defun azbyn/nothing ()
  (interactive)
  (message "not implemented"))
(defvar azbyn/goto-definition-function 'dumb-jump-go)
(defvar azbyn/search-symbol-function 'azbyn/nothing)
(defun azbyn/goto-definition ()
  (interactive)
  (call-interactively azbyn/goto-definition-function))
(defun azbyn/search-symbol ()
  (interactive)
  (call-interactively 'azbyn/goto-symbols-function))
(global-set-key (kbd "M-g d") 'azbyn/goto-definition)
(global-set-key (kbd "M-g q") 'azbyn/goto-quick-bookmark)
(global-set-key (kbd "M-g s") 'azbyn/search-symbol)

;; ** fill map
(define-prefix-command 'fill-key-map)
(define-key 'fill-key-map (kbd "s") 'set-fill-column)
(define-key 'fill-key-map (kbd "c l") 'center-line)
(define-key 'fill-key-map (kbd "c p") 'center-paragraph)
(define-key 'fill-key-map (kbd "r") 'fill-region)
(define-key 'fill-key-map (kbd "p") 'fill-region-as-paragraph)
(define-key 'fill-key-map (kbd "P") 'fill-paragraph)
;; ** azbyn's custom map
;; *** common
(define-prefix-command 'azbyn-key-map)
(defun azbyn/eval-buffer-or-region ()
  (interactive)
  (if mark-active
      (progn
        (call-interactively 'eval-region)
        (message "Evaluated region"))
    (eval-buffer)
    (message "Evaluated buffer")))


(define-key 'azbyn-key-map (kbd "<M-e>") 'azbyn/eval-buffer-or-region)
(define-key 'azbyn-key-map (kbd "<C-a>") 'flyspell-buffer)

(defun azbyn/align-current-or-region ()
  (interactive)
  (if mark-active
      (align)
    (align-current)))

(define-key 'azbyn-key-map (kbd "a") 'azbyn/align-current-or-region)

(bind-key  (kbd "<M-a> <RET>") 'org-open-at-point 'org-mode-map)

(define-key 'azbyn-key-map (kbd "q") 'quoted-insert)
(define-key 'azbyn-key-map (kbd "s") 'eshell)
(define-key 'azbyn-key-map (kbd "r") 'revert-buffer)

(define-key 'azbyn-key-map (kbd "u") 'upcase-dwim)
;;(define-key 'azbyn-key-map (kbd "d") 'downcase-dwim)
(define-key 'azbyn-key-map (kbd "l") 'downcase-dwim)

(define-key 'azbyn-key-map (kbd "TAB") 'ff-find-other-file)
(define-key 'azbyn-key-map (kbd "SPC") 'counsel-switch-buffer)

(define-key 'azbyn-key-map (kbd "C-SPC") 'azbyn/goto-quick-bookmark)
;; *** transpose
(define-key 'azbyn-key-map (kbd "t") 'my-c-transpose-args-backward)
(define-key 'azbyn-key-map (kbd "C-t") 'my-c-transpose-args-forward)
;; *** bookmarks
(define-prefix-command 'azbyn/bookmarks-map)
(defvar azbyn/bookmark-index 0)
(defun azbyn/quick-bookmark-impl()
  (bookmark-set (format "quick-%s" azbyn/bookmark-index)))
(defun azbyn/quick-bookmark()
  (interactive)
  (azbyn/quick-bookmark-impl)
  (message "Quick bookmark set"))

;;and set the other bookmark to previous point
;; kinda like exchange-mark-and-point
(defun azbyn/goto-quick-bookmark()
  (interactive)
  (let ((old-index azbyn/bookmark-index))
    (setq azbyn/bookmark-index (% (+ 1 azbyn/bookmark-index) 2))
    (azbyn/quick-bookmark-impl)
    (bookmark-jump (format "quick-%s" old-index))))


(define-key 'azbyn-key-map (kbd "<M-a>") 'azbyn/quick-bookmark)

(define-key 'azbyn-key-map (kbd "b") 'azbyn/bookmarks-map)

(define-key 'azbyn/bookmarks-map (kbd "q") 'azbyn/quick-bookmark)

(define-key 'azbyn/bookmarks-map (kbd "a") 'bookmark-set)
(define-key 'azbyn/bookmarks-map (kbd "s") 'bookmark-set)

(define-key 'azbyn/bookmarks-map (kbd "j") 'bookmark-jump)
(define-key 'azbyn/bookmarks-map (kbd "b") 'bookmark-jump)

(define-key 'azbyn/bookmarks-map (kbd "d") 'bookmark-delete)
;; *** open some =*buffer*=
(defvar azbyn/repl-name "*scratch*")

(defun azbyn/open-repl ()
  (interactive)
  (switch-to-buffer azbyn/repl-name))
(defun azbyn/open-help() (interactive) (switch-to-buffer "*Help*"))
(defun azbyn/open-scratch() (interactive) (switch-to-buffer "*scratch*"))

(defun azbyn/open-compilation() (interactive) (switch-to-buffer "*compilation*"))
(defun azbyn/open-messages() (interactive) (switch-to-buffer "*Messages*"))

(define-key 'azbyn-key-map (kbd "M-h") 'azbyn/open-help)
(define-key 'azbyn-key-map (kbd "M-s") 'azbyn/open-scratch)
(define-key 'azbyn-key-map (kbd "M-c") 'azbyn/open-compilation)
(define-key 'azbyn-key-map (kbd "M-r") 'azbyn/open-repl)
(define-key 'azbyn-key-map (kbd "m") 'azbyn/open-messages)
(define-key 'azbyn-key-map (kbd "i") 'ielm)
(define-key 'azbyn-key-map (kbd "j") 'evil-join)

(define-key 'azbyn-key-map (kbd "n") 'next-error)
(define-key 'azbyn-key-map (kbd "p") 'previous-error)

;; *** goto
(define-prefix-command 'azbyn/goto-map)
(define-prefix-command 'azbyn/edit-map)

(when azbyn/windows-mode
  (defconst azbyn/desktop (format "C:/Users/%s/Desktop/" user-login-name)))

(define-key 'azbyn-key-map (kbd "g") 'azbyn/goto-map)
(define-key 'azbyn-key-map (kbd "e") 'azbyn/edit-map)

(defun azbyn/goto-awesome()   (interactive) (dired "~/.config/awesome"))
(defun azbyn/goto-config()    (interactive) (dired "~/.config"))
(defun azbyn/goto-bin()       (interactive) (dired "~/bin"))
(if azbyn/windows-mode
    (defun azbyn/goto-desktop() (interactive) (dired 'azbyn/desktop))
  (defun azbyn/goto-downloads() (interactive) (dired "~/Downloads")))

(defun azbyn/goto-dotfiles()  (interactive) (dired "~/dotfiles"))
(defun azbyn/goto-emacs()     (interactive) (dired "~/.emacs.d"))
(defun azbyn/goto-git-dir()   (interactive) (dired "~/Git"))
(defun azbyn/goto-home()      (interactive) (dired "~"))

(if azbyn/windows-mode
    (defun azbyn/goto-perforce()  (interactive) (dired "D:/p4v/workspace"))
  (defun azbyn/goto-projects()  (interactive) (dired "~/Projects")))
(defun azbyn/goto-music()     (interactive) (dired "~/Music"))

(define-key 'azbyn/goto-map (kbd "a") 'azbyn/goto-awesome)
(define-key 'azbyn/goto-map (kbd "b") 'azbyn/goto-bin)
(define-key 'azbyn/goto-map (kbd "c") 'azbyn/goto-config)
(if azbyn/windows-mode
    (define-key 'azbyn/goto-map (kbd "d") 'azbyn/goto-desktop)
  (define-key 'azbyn/goto-map (kbd "d") 'azbyn/goto-downloads))

(define-key 'azbyn/goto-map (kbd "D") 'azbyn/goto-dotfiles)
(define-key 'azbyn/goto-map (kbd "C-d") 'azbyn/goto-dotfiles)


(define-key 'azbyn/goto-map (kbd "e") 'azbyn/goto-emacs)
(define-key 'azbyn/goto-map (kbd "g") 'azbyn/goto-git-dir)
(define-key 'azbyn/goto-map (kbd "h") 'azbyn/goto-home)
(if azbyn/windows-mode
    (define-key 'azbyn/goto-map (kbd "p") 'azbyn/goto-perforce)
  (define-key 'azbyn/goto-map (kbd "p") 'azbyn/goto-projects))

(unless azbyn/windows-mode
  (define-key 'azbyn/goto-map (kbd "m") 'azbyn/goto-music)

  ;; (defun azbyn/edit-bashrc()     (interactive) (find-file "~/.bashrc"))
  (defun azbyn/edit-xresources() (interactive) (find-file "~/.Xresources"))
  ;; (defun azbyn/edit-fish()       (interactive) (find-file "~/.config/fish/config.fish"))
  (defun azbyn/edit-zshrc()      (interactive) (find-file "~/.zshrc"))
  (defun azbyn/edit-p10rc()      (interactive) (find-file "~/.p10k.zsh"))
  (defun azbyn/edit-xinit()      (interactive) (find-file "~/.xinitrc"))
  (defun azbyn/edit-xmodmap()    (interactive) (find-file "~/.xmodmaprc"))
  (defun azbyn/edit-ranger()     (interactive) (find-file "~/.config/ranger/rc.conf"))

  (defun azbyn/edit-awesome-rc()          (interactive) (find-file "~/.config/awesome/rc.lua"))
  (defun azbyn/edit-awesome-keybindings() (interactive) (find-file "~/.config/awesome/keybindings.lua"))
  (defun azbyn/edit-awesome-utils()       (interactive) (find-file "~/.config/awesome/utils.lua"))
  (defun azbyn/edit-awesome-config()      (interactive) (find-file "~/.config/awesome/config.lua"))
  (defun azbyn/edit-awesome-theme()       (interactive) (find-file "~/.config/awesome/theme.lua"))
  (defun azbyn/edit-awesome-widgets()     (interactive) (find-file "~/.config/awesome/widgets/"))

  (defun azbyn/dotfile-make()     (interactive) (find-file "~/dotfiles/Makefile"))

  (define-key 'azbyn/edit-map (kbd "b") 'azbyn/goto-bin)
  ;; (define-key 'azbyn/edit-map (kbd "b") 'azbyn/edit-bashrc)
  ;; (define-key 'azbyn/edit-map (kbd "f") 'azbyn/edit-fish)
  (define-key 'azbyn/edit-map (kbd "z") 'azbyn/edit-zshrc)
  (define-key 'azbyn/edit-map (kbd "p") 'azbyn/edit-p10rc)

  (define-key 'azbyn/edit-map (kbd "x r") 'azbyn/edit-xresources)
  (define-key 'azbyn/edit-map (kbd "x i") 'azbyn/edit-xinit)
  (define-key 'azbyn/edit-map (kbd "x m") 'azbyn/edit-xmodmap)

  (define-key 'azbyn/edit-map (kbd "r") 'azbyn/edit-xresources)
  (define-key 'azbyn/edit-map (kbd "i") 'azbyn/edit-xinit)
  (define-key 'azbyn/edit-map (kbd "m") 'azbyn/edit-xmodmap)

  (define-key 'azbyn/edit-map (kbd "R") 'azbyn/edit-ranger)

  (define-key 'azbyn/edit-map (kbd "a r") 'azbyn/edit-awesome-rc)
  (define-key 'azbyn/edit-map (kbd "a k") 'azbyn/edit-awesome-keybindings)
  (define-key 'azbyn/edit-map (kbd "a u") 'azbyn/edit-awesome-utils)
  (define-key 'azbyn/edit-map (kbd "a c") 'azbyn/edit-awesome-config)
  (define-key 'azbyn/edit-map (kbd "a t") 'azbyn/edit-awesome-theme)
  (define-key 'azbyn/edit-map (kbd "a w") 'azbyn/edit-awesome-widgets)

  (define-key 'azbyn/edit-map (kbd "d m") 'azbyn/dotfile-make)
  )


(define-key 'azbyn-key-map (kbd "C-c") 'config-visit)
(define-key 'azbyn-key-map (kbd "C-S-c") 'config-visit-readonly)
(define-key 'azbyn-key-map (kbd "<C-e>") 'config-visit)
(define-key 'azbyn-key-map (kbd "C-S-e") 'config-visit-readonly)

(define-key 'azbyn-key-map (kbd "C-k") 'keybindings-visit)
(define-key 'azbyn-key-map (kbd "C-S-k") 'keybindings-visit-readonly)

(if azbyn/windows-mode
    (define-key 'azbyn-key-map (kbd "M-t") 'neotree-toggle)
  (defun azbyn/edit-todo () (interactive) (find-file "~/todo.org"))
  (define-key 'azbyn-key-map (kbd "M-t") 'azbyn/edit-todo))

(if azbyn/windows-mode
    (progn
      (defun azbyn/edit-misc () (interactive)
             (find-file (concat azbyn/desktop "misc.org")))
      (define-key 'azbyn-key-map (kbd "M-f") 'azbyn/edit-misc))
  (defun azbyn/edit-food () (interactive) (find-file "~/food.org"))
  (define-key 'azbyn-key-map (kbd "M-f") 'azbyn/edit-food))

(unless azbyn/windows-mode
  (defun azbyn/edit-movies () (interactive) (find-file "~/movies.org"))
  (define-key 'azbyn-key-map (kbd "M-m") 'azbyn/edit-movies))

;; *** spell checking bindings
(define-prefix-command 'azbyn/spellcheck-map)

(define-key 'azbyn-key-map (kbd "S") 'azbyn/spellcheck-map)

(define-key 'azbyn/spellcheck-map (kbd "b") 'flyspell-buffer)
(define-key 'azbyn/spellcheck-map (kbd "B") 'ispell-buffer)
(define-key 'azbyn/spellcheck-map (kbd "w") 'ispell-word)
(define-key 'azbyn/spellcheck-map (kbd "d") 'ispell-change-dictionary)
(define-key 'azbyn/spellcheck-map (kbd "s") 'flyspell-mode)
(define-key 'azbyn/spellcheck-map (kbd "k") 'ispell-kill-ispell)
(define-key 'azbyn/spellcheck-map (kbd "c") 'flyspell-correct-word-before-point)
;; *** input methods
(define-prefix-command 'azbyn/input-method-map)

(define-key 'azbyn-key-map (kbd "C-\\") 'azbyn/input-method-map)

(defun azbyn/set-input-old-romanian() (interactive) (set-input-method "old-romanian"))

(defun azbyn/set-input-russian() (interactive) (set-input-method "azbyn-russian-translit"))
(defun azbyn/set-input-azbyn-tex() (interactive) (set-input-method "azbyn-TeX"))
(defun azbyn/set-input-tex() (interactive) (set-input-method "TeX"))
(defun azbyn/set-input-hiragana() (interactive) (set-input-method "japanese-hiragana"))
(defun azbyn/set-input-katakana() (interactive) (set-input-method "japanese-katakana"))
(defun azbyn/set-input-japanese() (interactive) (set-input-method "japanese"))

(define-key 'azbyn/input-method-map (kbd "o") 'azbyn/set-input-old-romanian)
(define-key 'azbyn/input-method-map (kbd "r") 'azbyn/set-input-russian)
(define-key 'azbyn/input-method-map (kbd "t") 'azbyn/set-input-azbyn-tex)
(define-key 'azbyn/input-method-map (kbd "T") 'azbyn/set-input-tex)
(define-key 'azbyn/input-method-map (kbd "h") 'azbyn/set-input-hiragana)
(define-key 'azbyn/input-method-map (kbd "k") 'azbyn/set-input-katakana)
(define-key 'azbyn/input-method-map (kbd "j") 'azbyn/set-input-japanese)

;; ** diff
(use-package vdiff
  :ensure t)
(require 'diff-region)

(define-prefix-command 'diff-key-map)
(define-key 'azbyn-key-map (kbd "d") 'diff-key-map)

(define-key 'azbyn-key-map (kbd "c") 'azbyn/cyrillic-preview-mode)

(define-key 'diff-key-map (kbd "a") 'diff-region)
(define-key 'diff-key-map (kbd "b") 'diff-region-now)
(define-key 'diff-key-map (kbd "d") 'vdiff-buffers)
(define-key 'diff-key-map (kbd "f") 'vdiff-files)
(defun azbyn/vdiff-quit ()
  "quit vdiff and winner undo "
  (interactive)
  (vdiff-quit)
  (winner-undo))
(define-key 'diff-key-map (kbd "q") 'azbyn/vdiff-quit)
;; * weird movement
;; ** use C-i C-j for horizontal movement
(define-key input-decode-map "\C-i" [C-i])
(define-key input-decode-map "\C-j" [C-j])

(global-set-key [C-j] 'next-line)
(global-set-key [C-i] 'previous-line)

;;TODO
(defun azbyn/minibuffer-movement ()
  (interactive)
  (local-set-key [C-i] 'previous-line-or-history-element)
  (local-set-key [C-j] 'next-line-or-history-element))
(add-hook 'minibuffer-inactive-mode-hook 'azbyn/minibuffer-movement)

;; ** scrolling
(defun azbyn/scroll-up() (interactive) (forward-line -10))
(defun azbyn/scroll-down() (interactive) (forward-line 10))

;; ** delete window
(global-set-key (kbd "M-Q") 'delete-window)
;; ** exchange point and mark
(global-set-key (kbd "M-P") 'exchange-point-and-mark)
;; ** mark related
(defun azbyn/select-downwards ()
  (interactive)
  (set-mark (point-at-bol))
  (end-of-line)
  (forward-line 2))
(defun azbyn/select-upwards ()
  (interactive)
  (set-mark (point-at-eol))
  (beginning-of-line)
  (forward-line -1))
(defun azbyn/select-line()
  (interactive)
  (set-mark (point-at-bol))
  (end-of-line))
(defun azbyn/mark-to-eol()
  (interactive)
  (set-mark (point-at-eol)))

(defun azbyn/mark-whole-word()
  (interactive)
  (forward-char)
  (set-mark (azbyn/get-point 'azbyn/backward-word-begin))
  (azbyn/forward-word-end))

(defun azbyn/mark-to-eof()
  (interactive)
  (save-excursion
    (set-mark (azbyn/get-point 'end-of-buffer))))

(defun azbyn/mark-to-bof()
  (interactive)
  (save-excursion
    (set-mark (azbyn/get-point 'beginning-of-buffer))))

(define-prefix-command 'mark-key-map)
(global-set-key (kbd "M-m") 'mark-key-map)

(global-set-key (kbd "M-m <C-i>") 'azbyn/select-upwards)
(global-set-key (kbd "M-m <C-j>") 'azbyn/select-downwards)
;;select line
(global-set-key (kbd "M-m l") 'azbyn/select-line)

(global-set-key (kbd "M-m f") 'mark-defun)
(global-set-key (kbd "M-m s") 'exchange-point-and-mark)

(global-set-key (kbd "M-m p") 'mark-paragraph)
(global-set-key (kbd "M-m r") 'rectangle-mark-mode)
(global-set-key (kbd "M-m e") 'azbyn/mark-to-eol)
(global-set-key (kbd "M-m b") 'azbyn/mark-to-bol)
(global-set-key (kbd "M-m E") 'azbyn/mark-to-eof)
(global-set-key (kbd "M-m B") 'azbyn/mark-to-bof)

(global-set-key (kbd "M-m w") 'azbyn/mark-whole-word)

;; i know it's the default binding, it's just here to remind me to use it
(global-set-key (kbd "C-x SPC") 'rectangle-mark-mode)

(global-set-key (kbd "C-c v") 'rectangle-mark-mode)

;; ** fix eshell bindings
(defun azbyn/eshell-keys()
  (interactive)
  ;;(define-key

  ;;eshell-mode-map (kbd "C-a") nil)
  ;;(define-key eshell-mode-map (kbd "C-e") nil)
  ;;(define-key eshell-mode-map (kbd "C-q") 'eshell-bol)
  (define-key eshell-mode-map (kbd "M-I") 'eshell-previous-input)
  (define-key eshell-mode-map (kbd "M-J") 'eshell-next-input)

  ;;(define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input)
  ;;(define-key eshell-mode-map (kbd "M-n") 'eshell-next-input)
  (local-set-key (kbd "M-r") 'eshell-previous-input)
  (local-set-key (kbd "M-q") 'eshell-next-input)
  (local-set-key (kbd "M-k") (lambda ()
                               (interactive)
                               (eshell-bol)
                               (kill-line)))
  )
(add-hook 'eshell-mode-hook 'azbyn/eshell-keys)
;; ** keys that don't play nice
;; we don't straight up bind-key* because we want to get the original key
;; so for example C-q calls org-beginning-of-line in org mode
(define-key input-decode-map "\C-a" [C-a])
(define-key input-decode-map "\M-a" [M-a])
(define-key input-decode-map "\C-e" [C-e])
(define-key input-decode-map "\M-e" [M-e])

(define-key input-decode-map "\C-p" [C-p])
(define-key input-decode-map "\C-n" [C-n])

(define-key input-decode-map "\M-p" [M-p])
(define-key input-decode-map "\M-n" [M-n])


(global-set-key (kbd "M-p") 'move-lines-up)
(global-set-key (kbd "M-n") 'move-lines-down)

;; (define-key input-decode-map "\C-\M-e" [C-M-e])

(defun azbyn/get-og-key (key) (key-binding (kbd key)))
(defun azbyn/bind-key (key what)
  ;;i might want to change that to bind-key so that's why i use a wrapper
  (bind-key* key what))

;;(key-binding "C-a")

(azbyn/bind-key "C-x <C-e>" (azbyn/get-og-key "C-x C-e"))
(azbyn/bind-key "C-c <C-e>" (azbyn/get-og-key "C-c C-e"))

;; "C-x e" as kmacro-end-and-call-macro  is annoying
(azbyn/bind-key "C-x e" (azbyn/get-og-key "C-x C-e"))

(azbyn/bind-key "C-q" (azbyn/get-og-key "C-a"))
(azbyn/bind-key "C-r" (azbyn/get-og-key "C-e"))

(azbyn/bind-key "M-q" (azbyn/get-og-key "M-a"))
(azbyn/bind-key "M-r" (azbyn/get-og-key "M-e"))

(azbyn/bind-key "<C-a>" 'backward-char)
(azbyn/bind-key "C-f" 'forward-char)

;;M-j is set by c++-mode
(azbyn/bind-key "M-j" 'azbyn/scroll-down)
(azbyn/bind-key "M-i" 'azbyn/scroll-up)

(azbyn/bind-key "<prior>" 'azbyn/scroll-up)
(azbyn/bind-key "<next>" 'azbyn/scroll-down)

(azbyn/bind-key "M-I" (azbyn/get-og-key "M-p"))
(azbyn/bind-key "M-J" (azbyn/get-og-key "M-n"))

(azbyn/bind-key "<C-e>" 'azbyn/forward-word-begin)
(azbyn/bind-key "C-w" 'azbyn/backward-word-end)

(azbyn/bind-key "<M-e>" 'azbyn/forward-subword-begin)
(azbyn/bind-key "M-w" 'azbyn/backward-subword-end)

(azbyn/bind-key "C-M-e" 'azbyn/forward-subword-end)
(azbyn/bind-key "C-M-w" 'azbyn/backward-subword-begin)


(azbyn/bind-key "M-f" 'fill-key-map)
;;not really about fill, but related
(azbyn/bind-key "M-f M-t" 'toggle-truncate-lines)
(azbyn/bind-key "M-f t" 'toggle-truncate-lines)

(azbyn/bind-key "M-f M-w" 'toggle-truncate-lines)
(azbyn/bind-key "M-f w" 'toggle-truncate-lines)

;; M-f M-t and truncate lines is hard for me to remember
(defun azbyn/word-wrap()
  (interactive)
  (toggle-truncate-lines))


(azbyn/bind-key "<M-a>" 'azbyn-key-map)

(global-set-key (kbd "C-d") 'azbyn/delete-char-or-region)
(global-set-key (kbd "M-d") 'azbyn/kill-word)
(global-set-key (kbd "C-M-d") 'azbyn/kill-subword)
(global-set-key (kbd "C-S-d") 'azbyn/kill-subword)
(global-set-key (kbd "M-D") 'azbyn/kill-subword)
(defun azbyn/find-char ()
  (interactive)
  (message "Find char...")
  (call-interactively 'evil-find-char))

(defun azbyn/find-char-backward ()
  (interactive)
  (message "Find char backward...")
  (call-interactively 'evil-find-char-to-backward))

(azbyn/bind-key "<C-n>" 'azbyn/find-char)
(azbyn/bind-key "C-b" 'azbyn/find-char-backward)

(global-set-key (kbd "C-'") 'recenter-top-bottom)
;; ;;org mode likes rebinding C-k
(azbyn/bind-key "M-k" 'azbyn/kill-whole-line-or-append-region)
(azbyn/bind-key "C-M-k" 'append-next-kill)

(azbyn/bind-key "C-l" 'azbyn/copy-to-eol-or-region)
(global-set-key (kbd "C-S-l") 'azbyn/copy-whole-word)
(global-set-key (kbd "M-L") 'azbyn/copy-whole-subword)

(azbyn/bind-key "C-k" 'azbyn/kill-to-eol-or-region)
;; azbyn/define-key doesn't like lambdas and we can use global-set-key
(defun azbyn/yank-line-and-notify ()
  (interactive)
  (call-interactively 'evil-yank-line)
  (message "yanked line"))
(azbyn/bind-key (kbd "M-l") 'azbyn/yank-line-and-notify)

(azbyn/bind-key (kbd "C-S-k") 'azbyn/kill-whole-word)
(azbyn/bind-key (kbd "M-K") 'azbyn/kill-whole-subword)

(azbyn/bind-key "<C-p>" 'yank); azbyn/paste)
(azbyn/bind-key "C-S-p" 'azbyn/paste-before)
(azbyn/bind-key "<M-p>" 'azbyn/paste)
(azbyn/bind-key "M-P" 'azbyn/paste-before)

(azbyn/bind-key "M-b" 'evil-search-next)
(azbyn/bind-key "<M-n>" 'evil-search-previous)


(azbyn/bind-key "C-M-f" 'forward-sexp)
(azbyn/bind-key "C-M-a" 'backward-sexp)

(azbyn/bind-key "C-z" 'zop-up-to-char)

(azbyn/bind-key "C-v" 'yank);; azbyn/paste)
(azbyn/bind-key "C-S-v" 'azbyn/paste-before)

;; (use-package popup-kill-ring
;;   :ensure t)
;; (azbyn/bind-key "M-v"  'popup-kill-ring);; 'counsel-yank-pop)
(azbyn/bind-key "M-v" 'counsel-yank-pop)

;; ;(global-set-key (kbd "M-y") 'evil-join)

(azbyn/bind-key "M-`" 'evil-invert-char)
(azbyn/bind-key "C-2" 'evil-invert-char)

                                        ;(global-set-key (kbd "C-u") 'universal-argument)
(azbyn/bind-key "M-u" 'undo-tree-undo)
(azbyn/bind-key "M-U" 'undo-tree-redo)

(azbyn/bind-key "M-h" 'undo-tree-redo)
;; *** defun movement?
                                        ;(global-set-key (kbd "M-g") 'beginning-of-defun)
;;(global-set-key (kbd "M-g") 'end-of-defun)

(global-set-key (kbd "<C-tab>") 'er-switch-to-previous-buffer)

;; * macros
(global-set-key (kbd "C-9") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-0") 'kmacro-end-or-call-macro)
;; * window size
(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (+ 10 (face-attribute 'default :height))))
(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (+ -10 (face-attribute 'default :height))))

(defun default-font-size()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      107))
(global-set-key (kbd "H-+") 'default-font-size)
(global-set-key (kbd "H-=") 'increase-font-size)
(global-set-key (kbd "H--") 'decrease-font-size)

;; * code folding
(define-key undo-tree-map (kbd "C-_") nil)

(global-set-key (kbd "C-=") 'evil-open-fold)
(global-set-key (kbd "C-+") 'evil-open-fold-rec) ;; ctrl+shift+=

(global-set-key (kbd "C-M-=") 'evil-open-folds) ;; ctrl+alt+=
(global-set-key (kbd "C-M-+") 'evil-open-folds) ;; ctrl+alt+shift+=

(global-set-key (kbd "C--") 'evil-close-fold)
(global-set-key (kbd "C-_") 'evil-close-folds)   ;; ctrl+shift+-

;; * < and > to indent when mark is active mode
(defun azbyn/expand-region-to-lines ()
  (interactive)
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if (>= beg (point));; point is the at the beginning of the selection
          (progn
            (beginning-of-line)
            (set-mark (save-excursion
                        (goto-char end)
                        (point-at-eol))))
        (end-of-line)
        (set-mark (save-excursion
                    (goto-char beg)
                    (point-at-bol)))
        ))))

(defun azbyn/indent-base (fun)
  (azbyn/expand-region-to-lines)
  (let* ((mark (mark))
         (beg (region-beginning))
         (end (region-end))
         (begln (line-number-at-pos beg))
         (endln (line-number-at-pos end))
         (is-at-beg (>= beg (point))) ;; point is the at the beginning of the selection
         )
    (save-excursion
      (funcall fun beg end)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))
    (if is-at-beg
        (progn
          (goto-line begln)
          (beginning-of-line)
          (set-mark (save-excursion
                      (goto-line endln)
                      (point-at-eol))))
      (goto-line endln)
      (end-of-line)
      (set-mark (save-excursion
                  (goto-line begln)
                  (point-at-bol))))
    ))


(defun indent-or-< (cnt)
  (interactive "p")
  (if (region-active-p)
      (azbyn/indent-base 'evil-shift-left)
    ;; (indent-rigidly-left-to-tab-stop (region-beginning) (region-end))
    (self-insert-command cnt ?<)))

(global-set-key (kbd "<") 'indent-or-<)

(defun indent-or-> (cnt)
  (interactive "p")
  (if (region-active-p)
      (azbyn/indent-base 'evil-shift-right)
    (self-insert-command cnt ?>)))

(global-set-key (kbd "<") 'indent-or-<)
(global-set-key (kbd ">") 'indent-or->)

;; * other bindings and funcs
;; ** repeat
(global-set-key (kbd "H-.") 'repeat)

;; ** zop-up-to-char
(use-package zop-to-char
  :ensure t
  :config
  (global-set-key (kbd "C-z") 'zop-up-to-char)
  ;; (global-set-key [remap zap-up-to-char] 'zop-up-to-char)
  (setq zop-to-char-copy-keys '(?\C-l))
  (setq zop-to-char-kill-keys '(?\C-k))
  (setq zop-to-char-quit-at-point-keys '(?\C-g ?\e ?\r))
  (setq zop-to-char-quit-at-pos-keys '(?\C-q))
  ;; (setq zop-to-char-erase-keys '(?\C-d))
  (setq zop-to-char-delete-keys '(?\C-d))
  (setq zop-to-char-prec-keys '(left));; I've no idea how to use [C-a] here
  ;; (setq zop-to-char-next-keys '(right ?\C-f))
  )
;; ** find-char
;;  (use-package avy
;;    :ensure t
;;    :bind
;;      ("C-z" . avy-goto-char))

                                        ; (global-set-key (kbd "M-h") 'evil-find-char-to)
                                        ;  (global-set-key (kbd "M-S-h") 'evil-find-char-to-backward)
;; ** comment
(global-set-key (kbd "M-;") 'comment-line)
(use-package smart-comment
  :ensure t)
(azbyn/bind-key "C-;"  'smart-comment)

;; ** macros
(global-set-key (kbd "C-9") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-0") 'kmacro-end-or-call-macro)
;; ** grep
(define-key undo-tree-map (kbd "C-/") nil)
(defun azbyn/grep ()
  (interactive)

  ;;(grep-compute-defaults)
  (let ((command (read-shell-command "Grep command: "
                                     "grep . -rnI -e "
                                     'grep-history)))

    (grep--save-buffers)
    (compilation-start command 'grep-mode)))

(global-set-key (kbd "C-/") 'azbyn/grep)
;; ** quick-shell
(global-set-key (kbd "C-1") 'shell-command)
;; ** external open
(defun azbyn/external-open ()
  (interactive)
  (let ((path (read-file-name "External open: ")))
    (if (file-directory-p path)
        (message "We can't open a directory. (or we can but you probably don't want that)")
      ;; (async-start-process "azbyn-open" nil "xdg-run")
      ;; (async-shell-command (format "xdg-open \"%s\"" path))

      ;;i use ranger so i have that
      (make-process :name "external-open"
                    :buffer nil
                    :command (list "rifle" path)
                    ))))


(global-set-key (kbd "C-`") 'azbyn/external-open)
;; ** fixup indentation
(defun azbyn/fixup-indentation ()
  (interactive)
  (save-excursion
    (evil-shift-right (point-min) (point-max))
    (set-mark (point-min))
    (goto-char (point-max))
    (execute-kbd-macro (read-kbd-macro "<tab>"))
    (delete-trailing-whitespace)
    ))
;; for f in **/*.java { (progn (find-file f) (azbyn/fixup-indentation)) }
(global-set-key (kbd "M-t i") 'azbyn/fixup-indentation)
;; * compile
(global-set-key (kbd "M-c") 'azbyn/make-thing)

(global-set-key (kbd "M-C") 'compile)
;; * misc
;; ** escape as C-g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;;  (global-set-key (kbd "C-x ESC ESC") nil)
;;  (global-set-key [escape] (lambda ()
;;                                (interactive)
;;                                ;; (minibuffer-keyboard-quit)
;;                                (execute-kbd-macro (read-kbd-macro "C-g"))))

;; ** defun movement
(global-set-key (kbd "C-M-i") 'beginning-of-defun)
(global-set-key (kbd "C-M-j") 'end-of-defun)
;; ** top and bot
(define-key window-numbering-keymap (kbd "M-9") nil)
(define-key window-numbering-keymap (kbd "M-0") nil)

(global-set-key (kbd "M-9") 'beginning-of-buffer)
(global-set-key (kbd "M-0") 'end-of-buffer)
;; ** disable the binding in org mode and magit
(define-key org-mode-map (kbd "<C-tab>") nil)
(define-key org-mode-map (kbd "C-'") nil)
(define-key org-mode-map (kbd "C-<return>") nil); 'azbyn/cyr-prompt-replace-with-closest)


(when (boundp 'magit-status-mode-map)
  (define-key magit-status-mode-map (kbd "<C-tab>") nil))

;; ** =%=, =*=, =#= and other things
;;returns the whole word cursor is over
(defun azbyn/get-point (fun)
  (funcall fun)
  (point))
(defun azbyn/get-whole-word ()
  (save-excursion
    (let ((beg (azbyn/get-point 'azbyn/backward-word-begin))
          (end (azbyn/get-point 'azbyn/forward-word-end)))
      (buffer-substring-no-properties beg end))))

(defun azbyn/swiper-thing-at-point ()
  (interactive)
  (if mark-active
      (swiper (buffer-substring (mark) (point)))
    (swiper (azbyn/get-whole-word))))
;; (defun azbyn/swiper-thing-at-point-backward ()
;;   (interactive)
;;   (if mark-active
;;       (swiper))
;;   (swiper-backward (azbyn/get-whole-word)))

(global-set-key (kbd "C-5") 'evil-jump-item)
(global-set-key (kbd "C-3") 'azbyn/swiper-thing-at-point);;-backward)
(global-set-key (kbd "C-8") 'azbyn/swiper-thing-at-point)

;; * window movement and management
;; ** resizing and stuff
(define-prefix-command 'window-edit-key-map)
;; (global-set-key (kbd "M-O") window-edit-key-map)
(global-set-key (kbd "C-t") window-edit-key-map)

(global-set-key (kbd "C-t C-s") 'ace-swap-window)
(global-set-key (kbd "C-t C-t") 'recenter-top-bottom)

(smartrep-define-key
    global-map "C-t"
  '(("i" . evil-window-rotate-upwards)
    ("j" . evil-window-rotate-downwards)

    ;; ("a" . shrink-window-horizontally)
    ;; ("f" . enlarge-window-horizontally)

    ;; ("i" . evil-window-rotate-upwards)
    ;; ("j" . evil-window-rotate-downwards)

    ("<left>" . shrink-window-horizontally)
    ("<right>" . enlarge-window-horizontally)
    ("<up>" . enlarge-window)
    ("<down>" . shrink-window)

    ("=" . balance-windows)

    ("<C-up>" . evil-window-move-very-top)
    ("<C-down>" . evil-window-move-very-bottom)
    ("<C-left>" . evil-window-move-far-left)
    ("<C-right>" . evil-window-move-far-right)


    ;; ("M-i" . evil-window-move-very-top)
    ;; ("M-j" . evil-window-move-very-bottom)
    ;; ("M-a" . evil-window-move-far-left)
    ;; ("M-f" . evil-window-move-far-right)

    ("u" . winner-undo)
    ("r" . winner-redo)
    ("h" . winner-redo)

    ("o" . next-buffer)
    ("O" . previous-buffer)
    ))

(global-set-key (kbd "C-t d") 'delete-window)
(global-set-key (kbd "C-t q") 'delete-window)
(global-set-key (kbd "C-t k") 'kill-buffer-and-window)
(global-set-key (kbd "C-t s") 'server-edit)

(global-set-key (kbd "C-t t") 'toggle-transparency)

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key (kbd "C-t m") 'switch-to-minibuffer)
(global-set-key (kbd "C-t SPC") 'switch-to-minibuffer)
(global-set-key (kbd "C-t C-SPC") 'ivy-resume)


;; ** transparency
(define-prefix-command 'transparency-key-map)

(defun azbyn/the-transparency ()
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (numberp alpha) alpha 100)))
(defun azbyn/increase-transparency()
  (interactive)
  (let ((alpha (azbyn/the-transparency)))
    (unless (eq alpha 100)
      (set-frame-parameter nil 'alpha (+ alpha 5)))))
(defun azbyn/decrease-transparency()
  (interactive)
  (let ((alpha (azbyn/the-transparency)))
    (unless (eq alpha 5)
      (set-frame-parameter nil 'alpha (- alpha 5)))))
(defun azbyn/toggle-transparency()
  (interactive)
  (let ((alpha (azbyn/the-transparency )))
    (if (eq alpha 100)
        (set-frame-parameter nil 'alpha 95)
      (set-frame-parameter nil 'alpha 100))))
(global-set-key (kbd "C-t t") transparency-key-map)
(smartrep-define-key
    global-map "C-t t"
  '(("-" . azbyn/decrease-transparency)
    ("+" . azbyn/increase-transparency)
    ("=" . azbyn/increase-transparency)

    ("a" . azbyn/decrease-transparency)
    ("f" . azbyn/increase-transparency)

    ("i" . azbyn/decrease-transparency)
    ("j" . azbyn/increase-transparency)

    ("t" . azbyn/toggle-transparency)
    ))
;; ** previous
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-o") (lambda () (interactive)
                              (other-window -1)))
;; * end
(provide 'keybindings)
