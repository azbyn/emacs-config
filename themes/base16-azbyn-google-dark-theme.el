;; base16-google-dark-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Authors:
;; Scheme: Seth Wright (http://sethawright.com)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'azbyn-theme)

(defvar base16-azbyn-google-dark-colors
  '(:base00 "#1D1F21"
    :base01 "#282A2E"
    :base02 "#373B41"
    :base03 "#7E807E"
    :base04 "#B4B7B4"
    :base05 "#C5C8C6"
    :base06 "#E0E0E0"
    :base07 "#FFFFFF"
    :base08 "#CC342B"
    :base09 "#F96A38"
    :base0A "#FBA922"
    :base0B "#198844"
    :base0C "#3971ED"
    :base0D "#3971ED"
    :base0E "#A36AC7"
    :base0F "#FBA922"
    ))

;; Define the theme
(deftheme base16-azbyn-google-dark)

;; Add all the faces to the theme
(base16-theme-define-2 'base16-azbyn-google-dark
                     base16-azbyn-google-dark-colors)

;; Mark the theme as provided
(provide-theme 'base16-azbyn-google-dark)
(provide 'base16-azbyn-google-dark)

