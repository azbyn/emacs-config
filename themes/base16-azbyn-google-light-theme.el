;; base16-google-light-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Authors:
;; Scheme: Seth Wright (http://sethawright.com)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'azbyn-theme)

(defvar base16-azbyn-google-light-colors
  '(:base00 "#ffffff"
    :base01 "#e0e0e0"
    :base02 "#c5c8c6"
    :base03 "#8E908E";;#969896";; b4b7b4"
    :base04 "#474b41";; 7E807E";; 969896"
    :base05 "#373b41"
    :base06 "#282a2e"
    :base07 "#1d1f21"

    :base08 "#CC342B"
    :base09 "#F96A38"
    :base0A "#FBA922"
    :base0B "#198844"
    :base0C "#3971ED"
    :base0D "#3971ED"
    :base0E "#A36AC7"
    :base0F "#FBA922"

    ;; :base08 "#cc342b"
    ;; :base09 "#f96a38"
    ;; :base0A "#fba922"
    ;; :base0B "#198844"
    ;; :base0C "#3971ed"
    ;; :base0D "#3971ed"
    ;; :base0E "#a36ac7"
    ;; :base0F "#3971ed"
    ))

;; Define the theme
(deftheme base16-azbyn-google-light)

;; Add all the faces to the theme
(base16-theme-define-2 'base16-azbyn-google-light
                       base16-azbyn-google-light-colors)

;; Mark the theme as provided
(provide-theme 'base16-azbyn-google-light)
(provide 'base16-azbyn-google-light)
