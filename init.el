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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "ADBO" :family "DejaVu Sans Mono"))))
 '(flyspell-duplicate ((t (:underline (:color "#12A59C" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "#12A59C" :style wave)))))
 '(outline-4 ((t (:foreground "#3971ED"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FBA922"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#F96A38"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#EEB422"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#8B4513")))))
 ;; '(cursor ((t (:foreground "#FBA922"))))
;; '(popup-tip-face ((t (:background "#373B41" :foreground "#e0e0e0"))))
 ;; '(whitespace-trailing ((t (:background "#282A2E" :foreground "#FBA922"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b466c65fa5649e7b04140bf4acb2ac79c116d4525bd49a2d93ec32149005d1c2" "69968168ad233a5fb0dc0e996088f230c5ea5057c481cf040b234090a46d2bda" "20394187d82e245b58896668a33579500c5ce006a57e05aef30e5aef361dbb1e" "e8e216a0f4316ed83cdcab15996dcc1bcbf81e6e7812237f1a42aba062ea8e50" "41b5e1c6d5c2e58e40ecf2db7ac93c0a641a5f6e565ea4844ff99ab3a131def8" "e0efbd6a6153c38a25779bb544480ad394094fe3da717be32b722e5043195476" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" default))
 '(recenter-positions '(top middle bottom)))


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#1d1f21" :foreground "#c5c8c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "ADBO" :family "DejaVu Sans Mono"))))

 
;;  '(cursor ((t (:foreground "#FBA922"))))
;;  '(flyspell-duplicate ((t (:underline (:color "#12A59C" :style wave)))))
;;  '(flyspell-incorrect ((t (:underline (:color "#12A59C" :style wave)))))
;;  '(font-lock-comment-delimiter-face ((t (:foreground "#969896"))))
;;  '(line-number-current-line ((t (:background "#e0e0e0" :foreground "#373b41"))))
;;  '(outline-4 ((t (:foreground "#3971ED"))))
;;  '(popup-tip-face ((t (:background "#373B41" :foreground "#e0e0e0"))))
;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "#FBA922"))))
;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "#F96A38"))))
;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "#EEB422"))))
;;  '(rainbow-delimiters-depth-9-face ((t (:foreground "#8B4513"))))
;;  '(whitespace-trailing ((t (:background "#282A2E" :foreground "#FBA922")))))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" default)))
