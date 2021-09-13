;;; mu4easy-helm.el --- mu4easy-helm                 -*- lexical-binding: t; -*-

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

;; Setting up helm-mu package. 

;;; Code:
(require 'helm-mu)

(global-set-key (kbd "C-c h h c") 'helm-mu-contacts)

(define-key 'mu4e-main-mode-map    (kbd "s") 'helm-mu)
(define-key 'mu4e-headers-mode-map (kbd "s") 'helm-mu)
(define-key 'mu4e-view-mode-map    (kbd "s") 'helm-mu)

(provide 'mu4easy-helm)
;;; mu4easy-helm.el ends here
