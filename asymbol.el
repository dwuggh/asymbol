;;; asymbol.el --- fast symbol input method for latex and org mode inspired by cdlatex.el
;; Author: dwuggh
;; Maintainer: dwuggh
;; Version: 0.1
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;TODO commentary

;;; Code:

(require 'cl-lib)


(defgroup asymbol nil
  "fast inserting math symbols mainly for latex, org-mode"
  :tag "ASymbol"
  :prefix "asymbol/"
  :prefix "asymbol-"
  :group 'tex)


(defcustom asymbol-trigger-key ?`
  "Prefix key for `asymbol/insert-text-or-symbol'."
  :group 'asymbol
  :type '(choice
          (character)))

(defcustom asymbol-trigger-key-unicode ?\C-`
  "Prefix key for `asymbol/insert-text-or-symbol'."
  :group 'asymbol
  :type '(choice
          (character)))

(define-widget 'asymbol-symbol-alist 'lazy
  "an alist to describe symbols"
  :tag "ASymbol-symbol"
  :type '(repeat
          (list
           (character ?a)
           (repeat
            (list (string :tag "text") (string :tag "symbol")))))
  )

(define-widget 'asymbol-tag-alist 'lazy
  "an alist to describe tags (classification of symbols)"
  :tag "ASymbol-tag"
  :type '(repeat
          (list (character ?a)
                (repeat (list (string :tag "description")
                              (symbol :tag "tag list") (symbol :tag "symbol list")))))
  )

(defcustom asymbol-symbol-alist-unary-operators
  '(
    (?N ( "\\invneg" "⌐" ))
    (?n ( "\\neg" "¬" ))
    (?+ ( "\\pm" "±" ))
    (?x ( "\\dagger"  "†" ) ("\\ddagger"  "‡"))
    (?s ( "\\int"  "∫" ) ( "\\iint"  "∬" ) ( "\\iiint"  "∭" ))
    (?S ( "\\oint"  "∮" ) ( "\\oiint"  "∯" ) ( "\\oiiint"  "∰" ))
    (?c ( "\\intclockwise"  "∱" ))
    (?C ( "\\awint"  "⨑" ))
    (?v ( "\\varointclockwise"  "∲" ))
    (?V ( "\\ointctrclockwise"  "∳" ))

    )
  "unary operators. including some non-operators like ∫"
  :type 'asymbol-symbol-alist
  )

(defcustom asymbol-symbol-alist-binary-operators
  '(
    (?+ ( "\\pm"  "±" ))
    (?- ( "\\mp"  "∓" ))
    (?* ( "\\ast"  "∗" ) ("\\star"  "⋆"))
    (?x ( "\\times"  "×" ))
    (?d ( "\\div"  "÷" ))
    (?u ( "\\cup"  "∪" ) ("\\sqcap"  "⊓"))
    (?n ( "\\cap"  "∩" ) ("\\sqcup"  "⊔"))
    (?l ( "\\wedge"  "∧" ))
    (?v ( "\\vee"  "∨" ))
    (?L ( "\\land"  "∧" ))
    (?V ( "\\lor"  "∨" ))

    (?. ( "\\cdot"  "⋅" ) ("\\cdotp"  "·") ( "\\odot"  "⊙" ))

    
    
    (?o ( "\\oplus"  "⊕" ))
    (?O ( "\\ominus"  "⊖" ))
    (?i ( "\\otimes"  "⊗" ))
    (?I ( "\\oslash"  "⊘" ))
    )
  "binary operator symbols."
  :type 'asymbol-symbol-alist
  )


(defcustom asymbol-symbol-alist-top-level
  '(

   (?A ("\\Alpha"  "Α" ))
   (?a ( "\\alpha"  "α" ))
   
   (?B ( "\\Beta"  "Β" ))
   (?b ( "\\beta"  "β" ))

   (?G ( "\\Gamma"  "Γ" ))
   (?g ( "\\gamma"  "γ" ))
   
   (?D ( "\\Delta"  "Δ" ))
   (?d ( "\\delta"  "δ" ))
   
   (?E ( "\\Epsilon"  "Ε" ))
   (?e ( "\\epsilon"  "ε" ))
   
   (?Z ( "\\Zeta"  "Ζ" ))
   (?z ( "\\zeta"  "ζ" ))
   
   (?H ( "\\Eta"  "Η" ))
   (?h ( "\\eta"  "η" ))
   
   (?Q ( "\\Theta"  "Θ" ))
   (?q ( "\\theta"  "θ" ))
   
   (?I ( "\\Iota"  "Ι" ))
   (?i ( "\\iota"  "ι" ))
   
   (?K ( "\\Kappa"  "Κ" ))
   (?k ( "\\kappa"  "κ" ) ("\\varkappa" "ϰ"))
   
   (?L ( "\\Lambda"  "Λ" ))
   (?l ( "\\lambda"  "λ" ))
   
   (?M ( "\\Mu"  "Μ" ))
   (?m ( "\\mu"  "μ" ))
   
   (?N ( "\\Nu"  "Ν" ))
   (?n ( "\\nu"  "ν" ))
   
   (?X ( "\\Xi"  "Ξ" ))
   (?x ( "\\xi"  "ξ" ))
   
   (?O ( "\\Omicron"  "Ο" ))
   (?o ( "\\omicron"  "ο" ))
   
   (?P ( "\\Pi"  "Π" ))
   (?p ( "\\pi"  "π" ))
   
   (?R ( "\\Rho"  "Ρ" ))
   (?r ( "\\rho"  "ρ" ) ("\\varrho" "ϱ"))
   
   
   (?S ( "\\Sigma"  "Σ" ))
   (?s ( "\\sigma"  "σ" ) ( "\\varsigma"  "ς" ))
   
   (?T ( "\\Tau"  "Τ" ))
   (?t ( "\\tau"  "τ" ))
   
   (?U ( "\\Upsilon"  "Υ" ))
   (?u ( "\\upsilon"  "υ" ))

   (?F ( "\\Phi"  "Φ" ))
   (?f ( "\\phi" "ϕ" ) ( "\\varphi"  "φ" ))

   (?X ( "\\Chi"  "Χ" ))
   (?x ( "\\chi"  "χ" ))
   
   (?P ( "\\Psi"  "Ψ" ))
   (?p ( "\\psi"  "ψ" ))
   
   (?W ( "\\Omega"  "Ω" ))
   (?w ( "\\omega"  "ω" ) ( "\\piv"  "ϖ" ))
   
   
   ;; ("\\theta2" "ϑ")
   ;; (( "\\upsih"  "ϒ" ))
   )
  "symbols shown on the top level for asymbol."
  :group 'asymbol
  :type 'asymbol-symbol-alist
  )

(defcustom asymbol-tag-alist-top-level
  '(
    (?1 ("unary operators" asymbol-symbol-alist-unary-operators asymbol-tag-alist-top-level))
    (?2 ("binary operators" asymbol-symbol-alist-binary-operators asymbol-tag-alist-top-level))
    (?3 ("set/logic notation"))
    (?4 ("relations"))
    (?5 ("delimiters"))
    (?6 ("standard functions"))
    (?7 ("arrows"))
    (?8 ("miscellaneous symbols"))
    (?9 ("others"))
    (?0 ("top" asymbol-symbol-alist-top-level asymbol-tag-alist-top-level))
    )
  "navigation tags shown on the top level for asymbol."
  :group 'asymbol
  :type 'asymbol-tag-alist
  )


;; copied from `cdlatex-use-fonts'
(defcustom asymbol-use-fonts t
  "if non-nil, use fonts in help buffer.
Font-lock must be loaded as well to actually get fontified display."
  :group 'asymbol
  :type '(boolean))

(defun asymbol/use-fonts ()
  ;; Return t if we can and want to use fonts.
  (and window-system
       asymbol-use-fonts
       (boundp 'font-lock-keyword-face)))

;; maybe there are better ways to write composite functions?
;; maybe use dash's `-compose'?
(defun asymbol/alist-max-layer (alist)
  "return maximum layers needed for this alist.
it is actually the max length of `(cdr alist)'.
if alist is nil, return 0."
  (if alist (reduce 'max (map 'list (lambda (el) (length (cdr el))) alist))
    0))


(defun asymbol/print-help-list (alist layer)
  "print help according to alist"
  (let ((cnt 0) (flock (asymbol/use-fonts)))
    (dolist (element alist)
      (if (= (% cnt 4) 0) (insert "\n"))
      (setq cnt (+ 1 cnt))
      (let* ((char (car element))
             (prop (nth layer (cdr element)))
             )
        (if prop
            (let* ((desc (car prop))
                   (symb (car (cdr prop)))
                   (pstr (substring (concat
                                     (when (and symb (stringp symb))
                                       (concat symb "     ")) desc "                    ") 0 20)))
              (when flock (put-text-property 0 20 'face 'font-lock-keyword-face pstr))
              (insert char "  " pstr))
          (insert char "  " "                    ")
          )
        ))
    (insert "\n")
    )
  )

(defun asymbol/update-help-buffer (title taglist symlist layer max-layer level-desc)
  "Update help windows for symbols.
Create one if the help does not exist."
  (if (get-buffer-window " *ASymbol Help*")
      (select-window (get-buffer-window " *ASymbol Help*"))
      (switch-to-buffer-other-window " *ASymbol Help*"))
  (erase-buffer)
  (insert title "\n\n")
  (insert "navigation bar\n")
  (insert "--------------------------------------------------------------------------------\n")
  (asymbol/print-help-list taglist layer)
  (insert "\nsymbols\n")
  (insert "--------------------------------------------------------------------------------\n")
  (asymbol/print-help-list symlist layer)
  (message (concat level-desc ": layer %d of %d") (+ 1 layer) max-layer)
  )

(defun asymbol/read-char-with-help (taglist symlist)
  "like `cdlatex-read-char-with-help', read a char from keyboard and provide help
taglist is the current alist of navigation tags.
symlist is the current alist of symbols.
start-layer is always 0 for now.
one layer is contains every symbol on current help buffer.
e.g. if we have: `(?f ( \"\\phi\" \"ϕ\" ) ( \"\\varphi\"  \"φ\"))', then
we need at least 2 layers to represent f fully.
layers are switched through `asymbol-trigger-key'
"
  (let (char
        value
        (level-desc "top level ")
        (layer 0)
        (max-layer (max (asymbol/alist-max-layer taglist)
                        (asymbol/alist-max-layer symlist))))
    ;; exit only when finally get a symbol and ready for insert, or just abort
    (catch 'exit
      (save-window-excursion
        (asymbol/update-help-buffer "asymbol help" taglist symlist layer max-layer level-desc)
        (while t
          (setq char (read-char))
          (cond
           ((or (= char ?\e) (= char ?\C-g)) (keyboard-quit))
           ((= char asymbol-trigger-key)
            (setq layer (% (+ 1 layer) max-layer))
            (asymbol/update-help-buffer "asymbol help" taglist symlist layer max-layer level-desc)
            )

           ;; rewrite help buffer for which tag points to
           ((setq value (assoc char taglist))
            (let* ((key (car value))
                   (tuple (nth layer (cdr value)))
                   (target (cdr tuple)))
              (setq taglist (or (symbol-value (car (cdr target))) asymbol-tag-alist-top-level))
              (setq symlist (symbol-value (car target)))
              (setq level-desc (car tuple))
              (setq max-layer (max (asymbol/alist-max-layer taglist)
                                   (asymbol/alist-max-layer symlist)))
              (asymbol/update-help-buffer "asymbol help" taglist symlist 0 max-layer level-desc)
              ;; (message (concat desc ": layer %d of %d") layer max-layer)
              ))

           ;; exit with `(text unicode-symbol)'
           ((setq value (assoc char symlist))
            (throw 'exit (nth layer (cdr value))))
           (t
            ))
          )))))

(defun asymbol/insert-text-or-symbol (&optional text-or-symbol tuple)
  "insert the text or symbol in tuple"
  (interactive)
  (or tuple
      (setq tuple (asymbol/read-char-with-help asymbol-tag-alist-top-level asymbol-symbol-alist-top-level)))
  (or text-or-symbol (setq text-or-symbol 'text))
  (case text-or-symbol
    ('text (insert (car tuple)))
    ('symbol (insert (car (cdr tuple)))))
  )

;; test
;; (asymbol/read-char-with-help asymbol-tag-alist-top-level asymbol-symbol-alist-top-level 0 3)
;; (asymbol/insert-text-or-symbol
;;  (asymbol/read-char-with-help asymbol-tag-alist-top-level asymbol-symbol-alist-top-level 0 3)
;;  'text)

;;; keybindings -----------------------------------------------------------------

(add-hook 'tex-mode-hook
          (lambda ()
            (define-key latex-mode-map (vector asymbol-trigger-key ) 'asymbol/insert-text-or-symbol)
            ))
(global-set-key (vector asymbol-trigger-key-unicode)
                (lambda () (interactive) (asymbol/insert-text-or-symbol 'symbol)))


(provide 'asymbol)

;;; asymbol.el ends here
