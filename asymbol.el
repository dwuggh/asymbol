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

(define-widget 'asymbol-symbol-alist 'repeat
  "an alist to describe symbols"
  :tag "ASymbol-symbol"
  :type '(repeat
          (list
           (character ?a)
           (repeat
            (list (string :tag "text") (string :tag "symbol")))))
  )

(define-widget 'asymbol-tag-alist 'repeat
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
    (?1 ("unary operators" asymbol-tag-alist-top-level asymbol-symbol-alist-unary-operators))
    (?2 ("binary operators" asymbol-tag-alist-top-level asymbol-symbol-alist-binary-operators))
    (?3 ("set/logic notation"))
    (?4 ("relations"))
    (?5 ("delimiters"))
    (?6 ("standard functions"))
    (?7 ("arrows"))
    (?8 ("miscellaneous symbols"))
    (?9 ("others"))
    (?0 ("top" asymbol-tag-alist-top-level asymbol-symbol-alist-top-level))
    )
  "navigation tags shown on the top level for asymbol."
  :group 'asymbol
  :type 'asymbol-tag-alist
  )

(defun asymbol/print-help-list (alist level)
  "print help according to alist"
  (let ((cnt 0))
    (dolist (element alist)
      (if (= (% cnt 4) 0) (insert "\n"))
      (setq cnt (+ 1 cnt))
      (let* ((char (car element))
             (prop (nth level (cdr element)))
             )
        (if prop
            (let* ((desc (car prop))
                   (symb (car (cdr prop)))
                   (pstr (substring (concat
                                     (when (and symb (stringp symb))
                                       (concat symb "     ")) desc "                    ") 0 20)))
              (insert char "  " pstr))
          (insert char "  " "                    ")
          )
        ))
    (insert "\n")
    )
  )

(defun asymbol/turn-on-help (title taglist symlist level)
  "Show help windows for symbols"
  (interactive)
  (let ((nopk 0))
    (if (get-buffer-window " *ASymbol Help*")
        (select-window (get-buffer-window " *ASymbol Help*"))
      (switch-to-buffer-other-window " *ASymbol Help*"))
    (erase-buffer)
    (insert title "\n\n")
    (insert "--------------------------------------------------------------------------------\n")
    (asymbol/print-help-list taglist level)
    (insert "--------------------------------------------------------------------------------\n")
    (asymbol/print-help-list symlist level)
    ))


(defun asymbol/read-char-with-help (taglist symlist start-level max-level)
  "read a char from keyboard and provide help"
  (let (char
        value
        (level start-level))
    ;; exit only when finally get a symbol and ready for insert, or just abort
    (catch 'exit
      (save-window-excursion
        (asymbol/turn-on-help "asdf" taglist symlist level)
        (while t
          (setq char (read-char))
          (cond
           ((= char ?\C-g) (keyboard-quit))
           ((= char asymbol-trigger-key)
            (setq level (% (+ 1 level) max-level))
            (asymbol/turn-on-help "asdf" taglist symlist level))

           ;; rewrite help buffer for which tag points to
           ((setq value (assoc char taglist))
            (let* ((key (car value))
                   (tuple (nth level (cdr value)))
                   (desc (car tuple))
                   (target (cdr tuple)))
              (setq taglist (symbol-value (car target)))
              (setq symlist (symbol-value (car (cdr target))))
              (setq level 0)
              (asymbol/turn-on-help "asdf" taglist symlist level)))

           ;; exit with `(text unicode-symbol)'
           ((setq value (assoc char symlist))
            (throw 'exit (nth level (cdr value))))
           (t
            ))
          )))))

(defun asymbol/insert-text-or-symbol (&optional tuple text-or-symbol)
  "insert the text or symbol in tuple"
  (interactive)
  (or tuple
      (setq tuple (asymbol/read-char-with-help asymbol-tag-alist-top-level asymbol-symbol-alist-top-level 0 3)))
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

(provide 'asymbol)

;;; .el ends here
