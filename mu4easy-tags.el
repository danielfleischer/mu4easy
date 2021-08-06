;;; mu4easy-tags.el --- Tags support for mu4e        -*- lexical-binding: t; -*-

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

;; Fork of the tags component of the mu4e-goodies project https://github.com/panjie/mu4e-goodies.
;;
;; Support for adding tags to messages. These are saved in the :tags keyword and can be
;; filtered. 


;;; Code:

;; Tags for emails (from info pages of mu4e)
;;--------------------------------------------------
(add-to-list 'mu4e-marks
             '(tag
               :char       ("g" . " ")
               :prompt     "gtag"
               :ask-target (lambda () (read-string "What tag do you want to add/remove(+/-): "))
               :action      (lambda (docid msg target)
                              (mu4e-action-retag-message msg target))))

(mu4e~headers-defun-mark-for tag)

;; Keys
(define-key mu4e-headers-mode-map (kbd "G") 'mu4e-headers-mark-for-tag)
(define-key-after (lookup-key mu4e-headers-mode-map [menu-bar headers])
  [mark-tag] '("Mark for tag" . mu4e-headers-mark-for-tag) 'mark-pattern)


;; Actions to add tags
(add-to-list 'mu4e-view-actions
             '("add/remove tags" . mu4e-action-retag-message) t)


;; Quickly add/remove/search tag (named QT**) in header/message view
;;--------------------------------------------------
(defvar mu4e-goodies~quick-tag "QT**"
  "Quick tag.")

(defun mu4e-goodies-add-del-quick-tag ()
  "Quickly add/del tags."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (oldtags (mu4e-message-field msg :tags)))
    (if (member mu4e-goodies~quick-tag oldtags)
        (mu4e-action-retag-message msg (concat "-" mu4e-goodies~quick-tag))
      (mu4e-action-retag-message msg (concat "+" mu4e-goodies~quick-tag)))))


;; Show tags in the header view
(defface mu4e-goodies-face-tags
  '((((class color) (background light)) :weight bold :foreground "#8CD0D3")
    (((class color) (background  dark)) :weight bold :foreground "#8CD0D3"))
  "Face for show tags in header view."
  :group 'mu4easy)

(defun mu4e-goodies-headers-add-tags-to-subject (msg field val width)
  "Add tags to header view's subject field like: [TAG] Subject..."
  (if (eq field :subject)
      (let ((tags (mu4e-message-field msg :tags)))
        (if tags
            (setq val (concat
                       (mapconcat (function (lambda (x) (propertize (concat "[" x "]") 'face 'mu4e-goodies-face-tags)))
                                  tags "")
                       " "
                       val))
          val))
    val))


;; for mu4e 1.4.15 we can draw the tags in header view
(when (version<= mu4e-mu-version "1.4.15")
  (add-to-list 'mu4e~headers-field-handler-functions 'mu4e-goodies-headers-add-tags-to-subject))

(provide 'mu4easy-tags)
;;; mu4easy-tags.el ends here
