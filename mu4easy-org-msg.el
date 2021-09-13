;;; mu4easy-org-msg.el --- Org-Msg Integration       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Daniel Fleischer

;; Author: Daniel Fleischer <danflscr@gmail.com>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Integration of Org-Msg, see https://github.com/jeremy-compostella/org-msg.

;;; Code:

(require 'org-msg)

(setq
 org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:imagemagick"
 org-msg-startup "hidestars indent inlineimages"
 org-msg-default-alternatives '((new	    	. (html text))
      			                (reply-to-html	. (html text))
      			                (reply-to-text	. (text)))
 org-msg-convert-citation t)

(setq org-msg-signature "Daniel Fleischer")
(setq org-msg-greeting-fmt "Hi%s,\n\n")

(defcustom mu4easy-org-msg-font "\"Arial\"" "Font")
(defcustom mu4easy-org-msg-font-size "12pt" "Size")
(defcustom mu4easy-org-msg-line-height "16pt" "Height")

(customize-set-variable org-msg-enforce-css
                        (let* ((font-family `(font-family . ,mu4easy-org-msg-font)
                               (font-size `(font-size . ,mu4easy-org-msg-font-size))
                               (font `(,font-family ,font-size))
                               (line-height `(line-height . ,mu4easy-org-msg-line-height))
                               (bold '(font-weight . "bold"))
                               (theme-color "#0071c5")
                               (color `(color . ,theme-color))
                               (table `(,@font (margin-top . "0px")))
                               (ftl-number `(,@font ,color ,bold (text-align . "left")))
                               (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
                                                   fundamental ini json makefile man org plantuml
                                                   python sh xml))
                               (inline-src `((color . ,(face-foreground 'default))
                                             (background-color . ,(face-background 'default))))
                               (code-src
                                (mapcar (lambda (mode)
                                          `(code ,(intern (concat "src src-" (symbol-name mode)))
                                                 ,inline-src))
                                        inline-modes))
                               (base-quote '((padding-left . "5px") (margin-left . "10px")
                                             (margin-top . "10px") (margin-bottom . "0")
                                             (font-style . "italic") (background . "#f9f9f9")))
                               (quote-palette '("#324e72" "#6a3a4c" "#7a4900" "#ff34ff"
                                                "#ff4a46" "#008941" "#006fa6" "#a30059"
                                                "#ffdbe5" "#000000" "#0000a6" "#63ffac"))
                               (quotes
                                (mapcar (lambda (x)
                                          (let ((c (nth x quote-palette)))
                                            `(blockquote ,(intern (format "quote%d" (1+ x)))
                                                         (,@base-quote
                                                          (color . ,c)
                                                          (border-left . ,(concat "3px solid "
                                                                                  (org-msg-lighten c)))))))
                                        (number-sequence 0 (1- (length quote-palette))))))
                          `((del nil (,@font (color . "grey") (border-left . "none")
                                             (text-decoration . "line-through") (margin-bottom . "0px")
                                             (margin-top . "10px") (line-height . "11pt")))
                            (a nil (,color))
                            (a reply-header ((color . "black") (text-decoration . "none")))
                            (div reply-header ((padding . "3.0pt 0in 0in 0in")
                                               (border-top . "solid #e1e1e1 1.0pt")
                                               (margin-bottom . "20px")))
                            (span underline ((text-decoration . "underline")))
                            (li nil (,@font ,line-height (margin-bottom . "0px")
                                            (margin-top . "2px")))
                            (nil org-ul ((list-style-type . "square")))
                            (nil org-ol (,@font ,line-height (margin-bottom . "0px")
                                                (margin-top . "0px") (margin-left . "30px")
                                                (padding-top . "0px") (padding-left . "5px")))
                            (nil signature (,@font (margin-bottom . "20px")))
                            (blockquote quote0 ,(append base-quote '((border-left . "3px solid #ccc"))))
                            ,@quotes
                            (code nil (,font-size (font-family . "monospace") (background . "#f9f9f9")))
                            ,@code-src
                            (nil linenr ((padding-right . "1em")
                                         (color . "black")
                                         (background-color . "#aaaaaa")))
                            (pre nil ((line-height . "12pt")
                                      ,@inline-src
                                      (margin . "0px")
                                      (font-size . "9pt")
                                      (font-family . "monospace")))
                            (div org-src-container ((margin-top . "10px")))
                            (nil figure-number ,ftl-number)
                            (nil table-number)
                            (caption nil ((text-align . "left")
                                          (background . ,theme-color)
                                          (color . "white")
                                          ,bold))
                            (nil t-above ((caption-side . "top")))
                            (nil t-bottom ((caption-side . "bottom")))
                            (nil listing-number ,ftl-number)
                            (nil figure ,ftl-number)
                            (nil org-src-name ,ftl-number)
                            (table nil (,@table ,line-height (border-collapse . "collapse")))
                            (th nil ((border . "1px solid white")
                                     (background-color . ,theme-color)
                                     (color . "white")
                                     (padding-left . "10px") (padding-right . "10px")))
                            (td nil (,@table (padding-left . "10px") (padding-right . "10px")
                                             (background-color . "#f9f9f9") (border . "1px solid white")))
                            (td org-left ((text-align . "left")))
                            (td org-right ((text-align . "right")))
                            (td org-center ((text-align . "center")))
                            (div outline-text-4 ((margin-left . "15px")))
                            (div outline-4 ((margin-left . "10px")))
                            (h4 nil ((margin-bottom . "0px") (font-size . "11pt")
                                     ,font-family))
                            (h3 nil ((margin-bottom . "0px") (text-decoration . "underline")
                                     ,color (font-size . "12pt")
                                     ,font-family))
                            (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
                                     (font-style . "italic") ,color (font-size . "13pt")
                                     ,font-family))
                            (h1 nil ((margin-top . "20px")
                                     (margin-bottom . "0px") ,color (font-size . "14pt")
                                     ,font-family))
                            (p nil ((text-decoration . "none") (margin-bottom . "10px")
                                    (margin-top . "14px") ,line-height ,font-size
                                    ,font-family))
                            (div nil (,@font ,line-height))))))

(defcustom mu4easy-org-msg-fill-column 72 "Fill Column")

(add-hook 'org-msg-edit-mode-hook
          (lambda ()
            (set-fill-column mu4easy-org-msg-fill-column)
            (turn-on-auto-fill)
            (electric-indent-local-mode -1)
            (turn-on-flyspell)))

;; Calling the mode
(org-msg-mode)

(provide 'mu4easy-org-msg)
;;; mu4easy-org-msg.el ends here
