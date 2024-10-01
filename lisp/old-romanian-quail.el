;;; old-romanian-quail.el --- Quail package for TeX-style input -*-coding: utf-8;-*-

;; Copyright (C) 2001-2019 Free Software Foundation, Inc.
;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
;;   2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Author: TAKAHASHI Naoto <ntakahas@m17n.org>
;;         Dave Love <fx@gnu.org>
;; Keywords: multilingual, input, Greek, i18n

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'quail)

(defvar azbyn/cyr-default-z ?ꙁ)
(defvar azbyn/cyr-default-dz ?з)

;; https://en.wikipedia.org/wiki/Early_Cyrillic_alphabet

;; ' -> acute accent mark -> stressed silable
;; ` -> grave accent mark -> stressed last silable 
;; < -> psili -> soft breathing mark. Signals a word-initial vowel
;; " -> trema -> hiatus
;; * -> breve
;; \" -> "
;; \' -> '
;; \` -> `
;; \< -> <
;; ü -> ѵ (ijița)
;; y -> і
;; j -> й
;; ż, ź -> ж
;; î -> ꙟ
;; â -> ѫ
;; đ -> џ
;; w -> ѡ

;; (defvar azbyn/cyr-default-z ?з)
;; (defvar azbyn/cyr-default-dz ?ꙁ)

(eval-when-compile
  (require 'cl-lib)

  (defconst latin-ltx--mark-map
    '(("DOT BELOW" . "d")
      ("DOT ABOVE" . ".")
      ("OGONEK" . "k")
      ("CEDILLA" . "c")
      ("CARON" . "v")
      ;; ("HOOK ABOVE" . ??)
      ("MACRON" . "=")
      ("BREVE" . "u")
      ("TILDE" . "~")
      ("GRAVE" . "`")
      ("CIRCUMFLEX" . "^")
      ("DIAERESIS" . "\"")
      ("DOUBLE ACUTE" . "H")
      ("ACUTE" . "'")))

  (defconst latin-ltx--mark-re (regexp-opt (mapcar #'car latin-ltx--mark-map)))

  (defun latin-ltx--ascii-p (char)
    (and (characterp char) (< char 128)))

  (defmacro latin-ltx--define-rules (&rest rules)
    (load "uni-name")
    (let ((newrules ()))
      (dolist (rule rules)
        (pcase rule
          (`(,_ ,(pred characterp)) (push rule newrules)) ;; Normal quail rule.
          (`(,seq ,re)
           (let ((count 0)
                 (re (eval re t)))
             (maphash
              (lambda (name char)
                (when (and (characterp char) ;; Ignore char-ranges.
                           (string-match re name))
                  (let ((keys (if (stringp seq)
                                  (replace-match seq nil nil name)
                                (funcall seq name char))))
                    (if (listp keys)
                        (dolist (x keys)
                          (setq count (1+ count))
                          (push (list x char) newrules))
                      (setq count (1+ count))
                      (push (list keys char) newrules)))))
               (ucs-names))
             ;; (message "latin-ltx: %d mappings for %S" count re)
	     ))))
      (setq newrules (delete-dups newrules))
      (let ((rules (copy-sequence newrules)))
        (while rules
          (let ((rule (pop rules)))
            (when (assoc (car rule) rules)
              (let ((conflicts (list (cadr rule)))
                    (tail rules)
                    c)
                (while (setq c (assoc (car rule) tail))
                  (push (cadr c) conflicts)
                  (setq tail (cdr (memq c tail)))
                  (setq rules (delq c rules)))
                (message "Conflict for %S: %S"
                         (car rule) (apply #'string conflicts)))))))
      (let ((inputs (mapcar #'car newrules)))
        (setq inputs (delete-dups inputs))
        (message "latin-ltx: %d rules (+ %d conflicts)!"
                 (length inputs) (- (length newrules) (length inputs))))
      `(quail-define-rules ,@(nreverse newrules)))))

;; (defun azbyn/test-quail ()
;;   (message "test"))

;; (add-hook 'quail-activate-hook 'azbyn/test-quail)



(quail-define-package
 "old-romanian" "UTF-8" "Ꙟ" t
 "old romanian";;docstring

 `(("\t" . quail-completion)
   ;; (,(kbd "<Delete>") . nil)
   
   
   
   ;; (,(kbd "Ă") . "Ъ")

   );;translation keys
 t t nil nil nil nil nil nil nil
 ;; `(
 ;;   ;; (,(kbd "C") . ,(lambda () (message "oi") (self-insert-command "ъ")))
 ;;   (,(kbd "<Delete>") . nil)
 ;;   ;; (,(kbd "Ö") . azbyn/test-quail)
 ;;   ;; (" " . quail-no-conversion)
 ;;   ;; ("ö" ,(kbd "ö") . ,(lambda () (message "oiz") (self-insert-command "ъ")))
 ;;   ;; ()

 ;;   )
 t)

(latin-ltx--define-rules

 ("dz" ?з)
 ("Dz" ?З)
 ("DZ" ?З)
 ("z" ?ꙁ)
 ("Z" ?Ꙁ)

 ("_" ?҇)
 ("__" ?_)

 

 (">b" ?ⷠ) ;; пⷠ
 (">v" ?ⷡ) ;; пⷡ
 (">g" ?ⷢ) ;; пⷢ
 (">d" ?ⷣ) ;; пⷣ
 (">ù" ?ⷤ) ;; пⷤ #ż
 (">z" ?ⷥ) ;; пⷥ
 (">k" ?ⷦ) ;; пⷦ
 (">l" ?ⷧ) ;; пⷧ
 (">m" ?ⷨ) ;; пⷨ
 (">n" ?ⷩ) ;; пⷩ
 (">o" ?ⷪ) ;; пⷪ
 (">p" ?ⷫ) ;; пⷫ
 (">r" ?ⷬ) ;; пⷬ
 (">s" ?ⷭ) ;; пⷭ
 (">t" ?ⷮ) ;; пⷮ
 (">h" ?ⷯ) ;; пⷯ
 (">ț" ?ⷰ) ;; пⷰ
 (">þ" ?ⷰ) ;; пⷰ
 (">ć" ?ⷱ) ;; пⷱ
 
 (">ø" ?ⷱ) ;; пⷱ
 (">w" ?ꙻ) ;; пꙻ
 (">f" ?ꚞ) ;; пꚞ


 (">ș" ?ⷲ) ;; пⷲ
 (">șt" ?ⷳ) ;; пⷳ
 (">q" ?ⷳ) ;; пⷳ

 (">ç" ?ⷲ) ;; пⷲ
 (">çt" ?ⷳ) ;; пⷳ
  
 (">th" ?ⷴ) ;; пⷴ
 (">st" ?ⷵ) ;; пⷵ

 (">a" ?ⷶ) ;; пⷶ
 (">e" ?ⷷ) ;; пⷷ
 (">u" ?ⷹ) ;; пⷹ
 (">ea" ?ⷺ) ;; пⷺ
 (">iu" ?ⷻ) ;; пⷻ
 (">ia+" ?ⷼ) ;; пⷼ
 (">ia" ?ⷽ) ;; пⷽ
 (">â" ?ⷾ) ;; пⷾ
 (">ę" ?ꙴ) ;; пꙴ
 (">i" ?ꙵ) ;; пꙵ
 (">y" ?ꙶ) ;; пꙶ # ō
 (">u+" ?ꙷ) ;; пꙷ # ü
 (">ã" ?ꙸ) ;;ă ;; пꙸ
 (">\\" ?ꙺ) ;; пꙺ


 ;; ("%b" ?ⷠ) ;; пⷠ
 ;; ("%v" ?ⷡ) ;; пⷡ
 ;; ("%g" ?ⷢ) ;; пⷢ
 ;; ("%d" ?ⷣ) ;; пⷣ
 ;; ("%ż" ?ⷤ) ;; пⷤ
 ;; ("%z" ?ⷥ) ;; пⷥ
 ;; ("%k" ?ⷦ) ;; пⷦ
 ;; ("%l" ?ⷧ) ;; пⷧ
 ;; ("%m" ?ⷨ) ;; пⷨ
 ;; ("%n" ?ⷩ) ;; пⷩ
 ;; ("%o" ?ⷪ) ;; пⷪ
 ;; ("%p" ?ⷫ) ;; пⷫ
 ;; ("%r" ?ⷬ) ;; пⷬ
 ;; ("%s" ?ⷭ) ;; пⷭ
 ;; ("%t" ?ⷮ) ;; пⷮ
 ;; ("%h" ?ⷯ) ;; пⷯ
 ;; ("%ț" ?ⷰ) ;; пⷰ
 ;; ("%þ" ?ⷰ) ;; пⷰ
 ;; ("%ć" ?ⷱ) ;; пⷱ
 
 ;; ("%ø" ?ⷱ) ;; пⷱ


 ;; ("%ș" ?ⷲ) ;; пⷲ
 ;; ("%șt" ?ⷳ) ;; пⷳ

 ;; ("%ç" ?ⷲ) ;; пⷲ
 ;; ("%çt" ?ⷳ) ;; пⷳ
  
 ;; ("%th" ?ⷴ) ;; пⷴ
 ;; ("%st" ?ⷵ) ;; пⷵ

 ;; ("%a" ?ⷶ) ;; пⷶ
 ;; ("%e" ?ⷷ) ;; пⷷ
 ;; ("%u" ?ⷹ) ;; пⷹ
 ;; ("%ea" ?ⷺ) ;; пⷺ
 ;; ("%iu" ?ⷻ) ;; пⷻ
 ;; ("%ia+" ?ⷼ) ;; пⷼ
 ;; ("%ia" ?ⷽ) ;; пⷽ
 ;; ("%â" ?ⷾ) ;; пⷾ
 ;; ("%ę" ?ꙴ) ;; пꙴ
 ;; ("%i" ?ꙵ) ;; пꙵ
 ;; ("%y" ?ꙶ) ;; пꙶ # ō
 ;; ("%u+" ?ꙷ) ;; пꙷ # ü
 ;; ("%ã" ?ꙸ) ;;ă ;; пꙸ
 ;; ("%\\" ?ꙺ) ;; пꙺ

 
 ;; ("%w" ?ꙻ) ;; пꙻ
 ;; ("%~" ?꙼) ;; п꙼


 
 
 ("t+" ?ᲄ)

 ("đ" ?џ)
 ("Đ" ?Џ)

 ("ð" ?џ)
 ("Ð" ?Џ)
 
 ("ż" ?ж)
 ("Ż" ?Ж)
 
 ("ù" ?ж)
 ("Ù" ?Ж)

 ("ú+" ?з)
 ("Ú+" ?з)

 ("ú" ?ѕ)
 ("Ú" ?Ѕ)


 ;; ("[" ?ъ)
 ;; ("]" ?)

 ("ć" ?ч)
 ("Ć" ?Ч)

 ("ø" ?ч)
 ("Ø" ?Ч)

 ("č" ?ч)
 ("Č" ?Ч)

 ("c" ?к)
 ("C" ?К)
 ("k" ?к)
 ("K" ?К)

 ("șt" ?щ)
 ("Șt" ?Щ)

 ("çt" ?щ)
 ("Çt" ?Щ)
 ("ÇT" ?Щ)

 ("çþ" ?щ)
 ("Çþ" ?Щ)
 ("ÇÞ" ?Щ)





 ("\\" ?ь)
 ("]" ?ь)
 ;; ("\\\\" ?\\)

 ("--" ?—)

 ("ș" ?ш)
 ("Ș" ?Ш)
 
 ("ç" ?ш)
 ("Ç" ?Ш)


 ("q" ?щ)
 ("Q" ?Щ)
 
 ("qt" ?щ)
 ("Qt" ?Щ)

 ("ț" ?ц)
 ("Ț" ?Ц)
 ("þ" ?ц)
 ("Þ" ?Ц)

 ;; ("]" ?ц)



 ;; ("ę" ?є)
 ;; ("Ę" ?Є)

 ;; ("2" "оу")

 ;; ("ou" ?ѡ)
 ;; ("Ou" ?Ѡ)

 ("th" ?ѳ)
 ("Th" ?Ѳ)
 ("TH" ?Ѳ)

 ("pß" ?ѱ)
 ("Pß" ?Ѱ)

 ("ps+" ?ѱ)
 ("Ps+" ?Ѱ)

 ("dz+" ?ѕ)
 ("Dz+" ?Ѕ)
 ("DZ+" ?Ѕ)

 ;; ("q" ?з)
 ;; ("q" ?З)

 ("ź" ?ѕ)
 ("Ź" ?ѕ)

 ("x" ?ѯ)
 ("X" ?Ѯ)

 ("h" ?х)
 ("H" ?Х)

 ("f" ?ф)
 ("F" ?Ф)

 ("u" ?ꙋ)
 ("U" ?Ꙋ)

 ("t" ?т)
 ("T" ?Т)


 ("b" ?б)
 ("B" ?Б)

 ("v" ?в)
 ("V" ?В)

 ("g" ?г)
 ("G" ?Г)

 ("d" ?д)
 ("D" ?Д)



 ("j" ?й)
 ("J" ?Й)



 ("l" ?л)
 ("L" ?Л)

 ("m" ?м)
 ("M" ?М)

 ("n" ?н)
 ("N" ?Н)



 ("ü" ?ѵ)
 ("Ü" ?Ѵ)

 ("p" ?п)
 ("P" ?П)

 ("s" ?с)
 ("S" ?С)

 ("r" ?р)
 ("R" ?Р)


 ("ß" ?§)
 ("$" ?§)
 ("$+" ?҂)
 ;; ("ẞ" ?Ш)

 ("ßt" ?щ)
 ("ẞt" ?щ)

 
 ("^" ?̾)
 ("^+" ?꙽)

 ("@" ?ѕ)

 ("@+" ?̾)
 ("@++" ?꙽)
 
 ("'" ?́)
 ("\"" ?̈)
 ("\"\"" ?\")
 ("<" ?҆)

 ("`" ?̀)
 ("*" ?̆)
 ("*+" ?̑)
 ("**" ?*)

 ("~" ?҇)
 
 ("~+" ?҃)
 ("~~" ?҃)

 ("++" ?†)
 (".+" ?·)


 )

;;zero width space
(quail-defrule "e" ["е\u200B"])
(quail-defrule "E" ["Е\u200B"])

(quail-defrule "a" ["а\u200B"])
(quail-defrule "A" ["А\u200B"])

(quail-defrule "i" ["и\u200B"])
(quail-defrule "I" ["И\u200B"])

(quail-defrule "y" ["і\u200B"])
(quail-defrule "Y" ["І\u200B"])
(quail-defrule "o+" ["ѻ\u200B"])
(quail-defrule "O+" ["Ѻ\u200B"])

(quail-defrule "ö" ["ѡ\u200B"])
(quail-defrule "Ö" ["Ѡ\u200B"])

(quail-defrule "ö+" ["ѻ\u200B"])
(quail-defrule "Ö+" ["Ѻ\u200B"])

(quail-defrule "õ" ["ї"])
(quail-defrule "Õ" ["Ї"])
(quail-defrule "õ'" ["і́"])
(quail-defrule "Õ'" ["І́"])

(quail-defrule "iu" ["ю\u200B"])
(quail-defrule "Iu" ["Ю\u200B"])
(quail-defrule "IU" ["Ю\u200B"])
(quail-defrule "ia" ["ѧ\u200B"])
(quail-defrule "Ia" ["Ѧ\u200B"])

(quail-defrule "ia`" ["ѧ̀"])
(quail-defrule "Ia`" ["Ѧ̀"])

(quail-defrule "ia+" ["ꙗ\u200B"])
(quail-defrule "Ia+" ["Ꙗ\u200B"])
(quail-defrule "ea" ["ѣ\u200B"])
(quail-defrule "Ea" ["Ѣ\u200B"])
(quail-defrule "EA" ["Ѣ\u200B"])

(quail-defrule "w" ["ѡ\u200B"])
(quail-defrule "W" ["Ѡ\u200B"])
(quail-defrule "o" ["о\u200B"])
(quail-defrule "O" ["О\u200B"])

;; (quail-defrule "în" ["ꙟ\u200B"])
;; (quail-defrule "În" ["Ꙟ\u200B"])
;; (quail-defrule "îm" ["ꙟ\u200B"])
;; (quail-defrule "Îm" ["Ꙟ\u200B"])

(quail-defrule "îm+" ["ꙟⷨ"])
(quail-defrule "Îm+" ["Ꙟⷨ"])

(quail-defrule "îj" ["ꙟ̑й"])
(quail-defrule "îl" ["ꙟ̑л"])
(quail-defrule "își" ["ꙟ̑шй"])
;; (quail-defrule "Îj" ["Ꙟ̑й"])

(quail-defrule "î" ["ꙟ\u200B"])
(quail-defrule "Î" ["Ꙟ\u200B"])

(quail-defrule "î+" ["ꙟⷩ҇"])
(quail-defrule "Î+" ["Ꙟⷩ҇"])

(quail-defrule "ë" ["є᠎\u200B"]);;ę
(quail-defrule "Ë" ["Є᠎\u200B"]);;Ę

(quail-defrule "u+" ["оу\u200B"])
(quail-defrule "U+" ["Оу\u200B"])
;; (quail-defrule "ii'" ["і\u200Bи́"])

;; (quail-defrule "ii" ["і\u200Bй"])
(quail-defrule "ii" ["їй"])
(quail-defrule "jj" ["їй"])

(quail-defrule "ye" ["їе"])
(quail-defrule "ii'" ["їи́"])

(quail-defrule "ü+" ["у"])
(quail-defrule "Ü+" ["У"])
(quail-defrule "ã" ["ъ"])
(quail-defrule "Ã" ["Ъ"])
(quail-defrule "â" ["ѫ"])
(quail-defrule "Â" ["Ѫ"])

;; (keyboard-translate ?đ ?đ)

;; (quail-defrule "\\np" ["\\newpage %_"])
(quail-defrule "<>" ["<>"])
(quail-defrule "V+" ["Во́дъ"])
(quail-defrule "D+" ["Дими́трїе"])
(quail-defrule "Do+" ["До́мнꙋл"])
(quail-defrule "D+o" ["До́мнꙋл"])
(quail-defrule "K+" ["Кантими́р"])
;; (quail-defrule "îm+" ["ꙟⷨпър"])
;; (quail-defrule "î+m" ["ꙟⷨпър"])
(quail-defrule "Y+" ["І҆сторі́ѧ"])

(quail-defrule "a+" ["а҆ча́ста"])
(quail-defrule "f+" ["фїи́нд"])

(quail-defrule "Þ+" ["Ца́риград"])

(quail-defrule "cea`" ["ч̀ѣ"])

(quail-defrule "nu`" ["н̀ꙋ"])

(quail-defrule "ce`" ["ч̀е"])


(quail-defrule "Ci" ["Чи"])
(quail-defrule "CI" ["ЧИ"])
(quail-defrule "ci" ["чи"])
(quail-defrule "***" ["**"])
(quail-defrule "** _" ["* _"])
(quail-defrule "****" ["***"])



(quail-defrule "cii" ["чій"])
(quail-defrule "Cii" ["Чій"])

(quail-defrule "ciia\"" ["чїѧ"])
(quail-defrule "Ciia\"" ["Чїѧ"])



;; (quail-defrule "și" ["ій"])
(quail-defrule "iia\"" ["їѧ"])
(quail-defrule "Iia\"" ["Їѧ"])

(quail-defrule "iia" ["їѧ"])
(quail-defrule "Iia" ["Їѧ"])
              
(quail-defrule "cea" ["чѣ"])
(quail-defrule "cea+" ["ча"])
(quail-defrule "cia+" ["ча"])

(quail-defrule "Cea" ["Чѣ"])
(quail-defrule "ce" ["че"])
(quail-defrule "Ce" ["Че"])
(quail-defrule "ci" ["чи"])
(quail-defrule "Ci" ["Чи"])

(quail-defrule "cj" ["чй"])
(quail-defrule "Cj" ["Чй"])

(quail-defrule "chi" ["ки"])
(quail-defrule "Chi" ["Ки"])
              
(quail-defrule "chj" ["кй"])
(quail-defrule "Chj" ["Кй"])

(quail-defrule "chea" ["кѣ"])
(quail-defrule "Chea" ["Кѣ"])

(quail-defrule "che" ["ке"])
(quail-defrule "Che" ["Ке"])

(quail-defrule "gea" ["џѣ"])
(quail-defrule "Gea" ["Џѣ"])
;(quail-defrule "ge" ["џе"])
;(quail-defrule "Ge" ["Џе"])
;(quail-defrule "gi" ["џи"])
;(quail-defrule "Gi" ["Џи"])

(quail-defrule "giu" ["џю"])
(quail-defrule "Giu" ["Џю"])

(quail-defrule "ciu" ["чю"])
(quail-defrule "Ciu" ["Чю"])

(quail-defrule "ghi" ["ги"])
(quail-defrule "Ghi" ["Ги"])

(quail-defrule "ghea" ["гѣ"])
(quail-defrule "Ghea" ["Гѣ"])

(quail-defrule "ghe" ["ге"])
(quail-defrule "Ghe" ["Ге"])

;; (quail-defrule ">b~" ["ⷠ҇"]) ;; пⷠ
;; (quail-defrule ">v~" ["ⷡ҇"]) ;; пⷡ
;; (quail-defrule ">g~" ["ⷢ҇"]) ;; пⷢ
;; (quail-defrule ">d~" ["ⷣ҇"]) ;; пⷣ
;; (quail-defrule ">ż~" ["ⷤ҇"]) ;; пⷤ
;; (quail-defrule ">z~" ["ⷥ҇"]) ;; пⷥ
;; (quail-defrule ">k~" ["ⷦ҇"]) ;; пⷦ
;; (quail-defrule ">l~" ["ⷧ҇"]) ;; пⷧ
;; (quail-defrule ">m~" ["ⷨ҇"]) ;; пⷨ
;; (quail-defrule ">n~" ["ⷩ҇"]) ;; пⷩ
;; (quail-defrule ">o~" ["ⷪ҇"]) ;; пⷪ
;; (quail-defrule ">p~" ["ⷫ҇"]) ;; пⷫ
;; (quail-defrule ">r~" ["ⷬ҇"]) ;; пⷬ
;; (quail-defrule ">s~" ["ⷭ҇"]) ;; пⷭ
;; (quail-defrule ">t~" ["ⷮ҇"]) ;; пⷮ
;; (quail-defrule ">h~" ["ⷯ҇"]) ;; пⷯ
;; (quail-defrule ">ț~" ["ⷰ҇"]) ;; пⷰ
;; (quail-defrule ">þ~" ["ⷰ҇"]) ;; пⷰ
;; (quail-defrule ">ć~" ["ⷱ҇"]) ;; пⷱ
;; (quail-defrule ">ø~" ["ⷱ҇"]) ;; пⷱ
;; (quail-defrule ">ș~" ["ⷲ҇"]) ;; пⷲ
;; (quail-defrule ">șt~" ["ⷳ҇"]) ;; пⷳ
;; (quail-defrule ">ç~" ["ⷲ҇"]) ;; пⷲ
;; (quail-defrule ">çt~" ["ⷳ҇"]) ;; пⷳ

;; (quail-defrule ">th~" ["ⷴ҇"]) ;; пⷴ
;; (quail-defrule ">st~" ["ⷵ҇"]) ;; пⷵ
;; (quail-defrule ">a~" ["ⷶ҇"]) ;; пⷶ
;; (quail-defrule ">e~" ["ⷷ҇"]) ;; пⷷ
;; (quail-defrule ">u~" ["ⷹ҇"]) ;; пⷹ
;; (quail-defrule ">ea~" ["ⷺ҇"]) ;; пⷺ
;; (quail-defrule ">iu~" ["ⷻ҇"]) ;; пⷻ
;; (quail-defrule ">ia+~" ["ⷼ҇"]) ;; пⷼ
;; (quail-defrule ">ia~" ["ⷽ҇"]) ;; пⷽ
;; (quail-defrule ">â~" ["ⷾ҇"]) ;; пⷾ
;; (quail-defrule ">ę~" ["ꙴ҇"]) ;; пꙴ
;; (quail-defrule ">i~" ["ꙵ҇"]) ;; пꙵ
;; (quail-defrule ">y~" ["ꙶ҇"]) ;; пꙶ # ō
;; (quail-defrule ">u+~" ["ꙷ҇"]) ;; пꙷ # ü
;; (quail-defrule ">ã~" ["ꙸ҇"]) ;;ă ;; пꙸ
;; (quail-defrule ">\\~" ["ꙺ҇"]) ;; пꙺ
;; (quail-defrule ">w~" ["ꙻ҇"]) ;; пꙻ
;; (quail-defrule ">f~" ["ꚞ҇"]) ;; пꚞ

;; (quail-defrule "%0)" ["⁰⁾"])
;; (quail-defrule "%1)" ["¹⁾"])
;; (quail-defrule "%2)" ["²⁾"])
;; (quail-defrule "%3)" ["³⁾"])
;; (quail-defrule "%4)" ["⁴⁾"])
;; (quail-defrule "%5)" ["⁵⁾"])
;; (quail-defrule "%6)" ["⁶⁾"])
;; (quail-defrule "%7)" ["⁷⁾"])
;; (quail-defrule "%8)" ["⁸⁾"])
;; (quail-defrule "%9)" ["⁹⁾"])
;; (quail-defrule "%10)" ["¹⁰⁾"])

;; (quail-defrule "%0" ["⁰"])
;; (quail-defrule "%1" ["¹"])
;; (quail-defrule "%2" ["²"])
;; (quail-defrule "%3" ["³"])
;; (quail-defrule "%4" ["⁴"])
;; (quail-defrule "%5" ["⁵"])
;; (quail-defrule "%6" ["⁶"])
;; (quail-defrule "%7" ["⁷"])
;; (quail-defrule "%8" ["⁸"])
;; (quail-defrule "%9" ["⁹"])
;; (quail-defrule "%10" ["¹⁰"])


;; (quail-defrule "%_a" ["ᵃ"])
;; (quail-defrule "%_b" ["ᵇ"])
;; (quail-defrule "%_c" ["ᶜ"])
;; (quail-defrule "%_d" ["ᵈ"])
;; (quail-defrule "%_e" ["ᵉ"])
;; (quail-defrule "%_f" ["ᶠ"])
;; (quail-defrule "%_g" ["ᵍ"])
;; (quail-defrule "%_h" ["ʰ"])
;; (quail-defrule "%_i" ["ᶦ"])
;; (quail-defrule "%_j" ["ʲ"])
;; (quail-defrule "%_k" ["ᵏ"])
;; (quail-defrule "%_l" ["ˡ"])
;; (quail-defrule "%_m" ["ᵐ"])
;; (quail-defrule "%_m" ["ⁿ"])
;; (quail-defrule "%_o" ["ᵒ"])
;; (quail-defrule "%_p" ["ᵖ"])
;; ;; (quail-defrule "%_q" [""])
;; (quail-defrule "%_r" ["ʳ"])
;; (quail-defrule "%_s" ["ˢ"])
;; (quail-defrule "%_t" ["ᵗ"])
;; (quail-defrule "%_u" ["ᵘ"])
;; (quail-defrule "%_v" ["ᵛ"])
;; (quail-defrule "%_w" ["ʷ"])
;; (quail-defrule "%_x" ["ˣ"])
;; (quail-defrule "%_y" ["ʸ"])
;; (quail-defrule "%_z" ["ᶻ"])

(quail-defrule ">0)" ["⁰⁾"])
(quail-defrule ">1)" ["¹⁾"])
(quail-defrule ">2)" ["²⁾"])
(quail-defrule ">3)" ["³⁾"])
(quail-defrule ">4)" ["⁴⁾"])
(quail-defrule ">5)" ["⁵⁾"])
(quail-defrule ">6)" ["⁶⁾"])
(quail-defrule ">7)" ["⁷⁾"])
(quail-defrule ">8)" ["⁸⁾"])
(quail-defrule ">9)" ["⁹⁾"])
(quail-defrule ">10)" ["¹⁰⁾"])
(quail-defrule ">)" ["⁾"])

(quail-defrule ">0" ["⁰"])
(quail-defrule ">1" ["¹"])
(quail-defrule ">2" ["²"])
(quail-defrule ">3" ["³"])
(quail-defrule ">4" ["⁴"])
(quail-defrule ">5" ["⁵"])
(quail-defrule ">6" ["⁶"])
(quail-defrule ">7" ["⁷"])
(quail-defrule ">8" ["⁸"])
(quail-defrule ">9" ["⁹"])
(quail-defrule ">10" ["¹⁰"])


;; (quail-defrule " e" [" е҆"]); ["ᵃ"])
;; (quail-defrule "qea" ["ae"]); ["ᵃ"])

(quail-defrule ">>a" ["ᵃ"])
(quail-defrule ">>b" ["ᵇ"])
(quail-defrule ">>c" ["ᶜ"])
(quail-defrule ">>d" ["ᵈ"])
(quail-defrule ">>e" ["ᵉ"])
(quail-defrule ">>f" ["ᶠ"])
(quail-defrule ">>g" ["ᵍ"])
(quail-defrule ">>h" ["ʰ"])
(quail-defrule ">>i" ["ᶦ"])
(quail-defrule ">>j" ["ʲ"])
(quail-defrule ">>k" ["ᵏ"])
(quail-defrule ">>l" ["ˡ"])
(quail-defrule ">>m" ["ᵐ"])
(quail-defrule ">>m" ["ⁿ"])
(quail-defrule ">>o" ["ᵒ"])
(quail-defrule ">>p" ["ᵖ"])
;; (quail-defrule ">>q" [""])
(quail-defrule ">>r" ["ʳ"])
(quail-defrule ">>s" ["ˢ"])
(quail-defrule ">>t" ["ᵗ"])
(quail-defrule ">>u" ["ᵘ"])
(quail-defrule ">>v" ["ᵛ"])
(quail-defrule ">>w" ["ʷ"])
(quail-defrule ">>x" ["ˣ"])
(quail-defrule ">>y" ["ʸ"])
(quail-defrule ">>z" ["ᶻ"])
(quail-defrule "\\f" ["\\foreign"])


;; (quail-defrule "ã" ["wa"])
;; (quail-defrule "ãi" ["waii"])






;;TODO try converting stuff  to latin-1
;;see quail-input-method
(quail-defrule "ș" ["ш"])
(quail-defrule "șș" ["dz"])



(require 'quail-redone)


(define-minor-mode azbyn/old-romanian-extra
  "A minor mode to convert inserted Unicode characters
   that dont work in old-romanaian quail."
  :lighter "ъ"
  :keymap (let ((map (make-sparse-keymap)))
            ;; (define-key map (kbd "z") (lambda () (self-insert-command (char-to-string azbyn/cyr-default-z))))
            ;; (define-key map (kbd "z") (lambda () (self-insert-command (char-to-string (upcase azbyn/cyr-default-z)))))

            (define-key map (kbd "ș") (lambda () (interactive) (azbyn/input-utf-char ?ç)))
            (define-key map (kbd "Ș") (lambda () (interactive) (azbyn/input-utf-char ?Ç)))

            
            (define-key map (kbd "ă") (lambda () (interactive) (azbyn/input-utf-char ?ã))) ;; "ъ")
            (define-key map (kbd "Ă") (lambda () (interactive) (azbyn/input-utf-char ?Ã))) ;; "ъ")

    
            (define-key map (kbd "ț") (lambda () (interactive) (azbyn/input-utf-char ?þ)))
            (define-key map (kbd "Ț") (lambda () (interactive) (azbyn/input-utf-char ?Þ)))

            (define-key map (kbd "ę") (lambda () (interactive) (azbyn/input-utf-char ?ë)))
            (define-key map (kbd "Ę") (lambda () (interactive) (azbyn/input-utf-char ?Ë)))

            (define-key map (kbd "ć") (lambda () (interactive) (azbyn/input-utf-char ?ø)))
            (define-key map (kbd "Ć") (lambda () (interactive) (azbyn/input-utf-char ?Ø)))

            (define-key map (kbd "ż") (lambda () (interactive) (azbyn/input-utf-char ?ù)))
            (define-key map (kbd "Ż") (lambda () (interactive) (azbyn/input-utf-char ?Ù)))

            (define-key map (kbd "ź") (lambda () (interactive) (azbyn/input-utf-char ?ú)))
            (define-key map (kbd "Ź") (lambda () (interactive) (azbyn/input-utf-char ?Ú)))

            (define-key map (kbd "ō") (lambda () (interactive) (azbyn/input-utf-char ?õ)))
            (define-key map (kbd "Ō") (lambda () (interactive) (azbyn/input-utf-char ?Õ)))

            (define-key map (kbd "đ") (lambda () (interactive) (azbyn/input-utf-char ?ð)))
            (define-key map (kbd "Đ") (lambda () (interactive) (azbyn/input-utf-char ?Ð)))

            ;; (define-key map (kbd "ō") "ї")
            ;; (define-key map (kbd "Ō") "Ї")

            ;; (define-key map (kbd "đ") "џ")
            ;; (define-key map (kbd "Đ") "Џ")

            
            
            (define-key map (kbd "᠎") "") ;; mongolian vowel separator
            map))

(defvar azbyn/add-psili t)
(defun azbyn/toggle-psili ()
  (interactive)
  (setq azbyn/add-psili (not azbyn/add-psili)))

(defun azbyn/quail-add-psili ()
  
  ;; (message (format "quail stuff '%s'; '%s' '%c' '%c'" quail-current-key  quail-current-str
  ;;                  (char-before)
  ;;                  (char-after)))

  

  
  (when (and (char-before (- (point) 1) )
             azbyn/add-psili)
    (let ((curr (downcase (char-before)))
          (prev ;(downcase
                 (char-before (- (point) 1)))
          (prevv (char-before (- (point) 2)))
          (prevvv (char-before (- (point) 3)))
          (next (char-after))
          )
      ;; (message "psili stuff c'%s-%c' pv'%s-%c' pvv'%s-%c'" curr curr prev prev prevv prevv)
      ;; (when char-)
      (when (and (eq prev ?҆) (seq-contains-p "҆ⷶⷠⷡⷢⷣⷤⷥⷦⷧⷨⷩⷪⷫⷬⷭⷮⷯⷰⷰⷱⷱⷲⷳⷲⷳⷴⷵⷶⷷⷹⷺⷻⷼⷽⷾꙴꙵꙶꙷꙸꙺ҃̑̆" curr))
        ;; (message "delyeeto %c %s" curr curr )
        (delete-char -2)
        (insert-char curr)
        ;; (delete-char -1)
        )
      (when (and (eq prevv ?҆) (seq-contains-p "҇" curr))
        ;; (message "delyeeto %c %s" curr curr )
        (delete-char -3)
        (insert-char prev)
        (insert-char curr)
        ;; (delete-char -1)
        )
      (when (seq-contains-p "\u200b" curr)
        (delete-char -1)
        ;; (message "del mvs %x %c" next next)
        (when
            (and ;;(seq-contains-p "аеиіоѡꙋъѣюѧꙗѻꙟє" curr)     
             (or
              (and (or (seq-contains-p " \n" prevv) (not prevv))
                   (or (not (seq-contains-p "-" prevvv)) (not prevvv)))
              (and (or (seq-contains-p " \n" prevvv) (not prevv))
                   (seq-contains-p "Оо" prevv)
                   (seq-contains-p "Уу" prev)
                   )
              )
             
             ;; (not (seq-contains-p "ⷶⷠⷡⷢⷣⷤⷥⷦⷧⷨⷩⷪⷫⷬⷭⷮⷯⷰⷰⷱⷱⷲⷳⷲⷳⷴⷵⷶⷷⷹⷺⷻⷼⷽⷾꙴꙵꙶꙷꙸꙺ" curr))
             ;; (not (seq-contains-p "҃҆" curr))
             ;; (not (seq-contains-p "҃҆" curr))
             ;;mongolian vowel separator
             ;; (seq-contains-p "aeiouōîAEIOUŌÎ" (string-to-char quail-current-key))
             ;; (not (seq-contains-p "<~%>᠎" (string-to-char quail-current-key)))
             )
          ;; (message "oi")
          ;; (save-excursion
          ;; (delete-char -1)
          (insert-char ?҆))
          ;; (right-char)
          ;; (message "ozi")
          ;;     (message "hello advice '%c' " prev)
          )
        )
    ))

(add-hook 'input-method-after-insert-chunk-hook 'azbyn/quail-add-psili)
;; (advice-add
;;    'quail-terminate-translation    ; function to be advised
;;    :after            ; advice runs first
;;    'azbyn/quail-add-psili)
   ;; (lambda (&rest r) ;(delete-other-windows)


   ;;   ;; (message "hi")
   ;;   )
   ;; '((name . "add-psili")))

;; (advice-remove 'quail-terminate-translation "add-psili")

(defun azbyn/old-romanian-extra-enable ()
  (interactive)
  (azbyn/old-romanian-extra 1))
(defun azbyn/old-romanian-extra-disable ()
  (interactive)
  (azbyn/old-romanian-extra -1))

(defun azbyn/remove-zero-width ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\u200B" nil t)
      (replace-match ""))
    ))

(provide 'old-romanian-quail)
;;; old-romanian-quail.el ends here
