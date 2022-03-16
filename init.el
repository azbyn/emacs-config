;; highly inspired by Uncle Dave's config

;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 100000000 ;16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(when (< emacs-major-version 27) (package-initialize))

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; don't create the package-selected-packages variable
(defun package--save-selected-packages (&optional value)
  "Set and save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value)))

(add-to-list 'load-path "~/.emacs.d/config/")
(require 'config)
(require 'keybindings)

;; (when (file-readable-p "~/.emacs.d/config.el")
  ;; (load-file (expand-file-name "~/.emacs.d/config.el")))
;; (when (file-readable-p "~/.emacs.d/keybindings.el")
  ;; (load-file (expand-file-name "~/.emacs.d/keybindings.el")))

;; (when (file-readable-p "~/.emacs.d/config.org")
;;   (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
;; (when (file-readable-p "~/.emacs.d/keybindings.org")
;;   (org-babel-load-file (expand-file-name "~/.emacs.d/keybindings.org")))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(default ((t (:inherit nil :stipple nil :background "#1d1f21" :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height ,azbyn/height :width normal :foundry "ADBO" :family ,azbyn/font))))
 '(flyspell-duplicate ((t (:underline (:color "#12A59C" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "#12A59C" :style wave)))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#969896"))))
 '(line-number-current-line ((t (:background "#e0e0e0" :foreground "#373b41"))))
 '(outline-4 ((t (:foreground "#3971ED"))))
 '(popup-tip-face ((t (:background "#373B41" :foreground "#e0e0e0"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FBA922"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#F96A38"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#EEB422"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#8B4513"))))
 '(whitespace-trailing ((t (:background "#282A2E" :foreground "#FBA922")))))
