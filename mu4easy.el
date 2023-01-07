;;; mu4easy.el --- Opinionated collection of packages and configuration for using mu4e with multiple accounts    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Daniel Fleischer

;; Author: Daniel Fleischer;;  <danflscr@gmail.com>
;; Keywords: mail
;; Homepage: https://github.com/danielfleischer/mu4easy
;; Package-Version: 1.0
;; Package-Requires: ((emacs "25.1") (quelpa "1.0") (use-package "2"))

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

;; This package contains a collection of packages and configurations
;; making it easy and fun to use email with Emacs using mu, mu4e, mbsync
;; and other improvements. The setup supports multiple email providers
;; such as Google, Apple, GMX and Proton. In addition to this elisp
;; package, there is an mbsync configuration that needs to be changed
;; and copied manually.

;;; Code:
(require 'use-package)
(require 'quelpa)
(use-package quelpa-use-package
  :ensure t)

(defgroup mu4easy nil
  "Easy configuration for mu4e."
  :group 'mail)

(defcustom mu4easy-account '("Gmail" "GMX" "Apple" "Proton")
  "List of email accounts names, as defined on disk."
  :type '(repeat string))

(defcustom mu4easy-greeting "Hi%s,\n\n"
  "Email greeting where %s is the first name of 1-2 recipients.
See also `org-msg-greeting-fmt'."
  :type '(string))

(defcustom mu4easy-signature "\n\n*Daniel Fleischer*"
  "Signature; supports org syntax thanks to org-msg."
  :type '(string))

(defcustom mu4easy-maildir "~/Documents/Mail"
  "Location of maildirs; see mbsync configuration."
  :type '(directory))

(defcustom mu4easy-download-dir "~/Downloads"
  "Location of downloads dir."
  :type '(directory))

(setq mail-user-agent 'mu4e-user-agent)

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :bind (("C-c u" . mu4e)
         :map mu4e-main-mode-map
         ("x" . bury-buffer)
         ("I" . mu4e-update-index)
         ("U" . mu4easy-update-mail-and-index)))


(use-package smtpmail)

(use-package mu4e-icalendar
  :config
  (setq mu4e-icalendar-trash-after-reply nil
        mu4e-icalendar-diary-file diary-file)
  (mu4e-icalendar-setup))

(use-package mu4e-contrib
  :defer 2
  :bind (:map mu4e-headers-mode-map
              ("M" . mu4e-headers-mark-all)
              ("N" . mu4e-headers-mark-all-unread-read)))

(use-package mu4e-goodies-tags
  :quelpa (mu4e-goodies-tags :fetcher github
                             :repo "panjie/mu4e-goodies"
                             :files ("mu4e-goodies-tags.el"
                                     "mu4e-goodies-utils.el")))

(use-package mu4e-column-faces
  :ensure t
  :config (mu4e-column-faces-mode))

(use-package mu4e-alert
  :quelpa (mu4e-alert :fetcher github
                      :repo "xzz53/mu4e-alert")
  :config
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(use-package helm-mu
  :quelpa (helm-mu :fetcher github
                   :repo "danielfleischer/helm-mu")
  :bind
  (("C-c h h c" . 'helm-mu-contacts)
   (:map mu4e-search-minor-mode-map
         ("s" . helm-mu)))
  :config
  (setq helm-mu-append-implicit-wildcard nil
        helm-mu-gnu-sed-program "gsed"))

(use-package org-msg
  :ensure t
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:imagemagick"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-default-alternatives '((new           . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text)))
        org-msg-signature mu4easy-signature
        org-msg-greeting-fmt mu4easy-greeting
        org-msg-posting-style 'top-posting
        org-msg-greeting-name-limit 2
        org-msg-convert-citation t)
  (org-msg-mode))

(defun mu4easy-mail-link-description (msg)
  "Creating a link description to be used with `org-store-link'.
Argument MSG msg at point."
  (let ((subject (or (plist-get msg :subject)
                     "No subject"))
        (date (or (format-time-string mu4e-headers-date-format
                                      (mu4e-msg-field msg :date))
                  "No date"))
        (to-from (mu4e~headers-from-or-to msg)))
    (format "%s: %s (%s)" to-from subject date)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'mu4e-view-mode-hook
          (lambda ()
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(add-hook 'org-msg-edit-mode-hook
          (lambda ()
            (set-fill-column 120)
            (turn-on-auto-fill)
            (electric-indent-local-mode -1)
            (turn-on-flyspell )))

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (set-fill-column 120)
            (turn-on-auto-fill)
            (electric-indent-local-mode -1)
            (turn-on-flyspell)))

(add-hook 'mu4e-main-mode-hook
          (lambda () (text-scale-decrease 1)))

(add-to-list 'mu4e-view-actions
             '("Apply Email" . mu4e-action-git-apply-mbox) t)

(add-hook 'mu4e-context-changed-hook
          (lambda ()
            (when (derived-mode-p 'mu4e-main-mode)
              (revert-buffer))))

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update specific accounts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mu4easy-update-custom-account ()
  "Run mbsync update for a specific account."
  (interactive)
  (let ((account (completing-read
                  "Select account: "
                  (cons "All" mu4easy-mail-accounts) nil t nil nil "All"))
        (command (format "INSIDE_EMACS=%s mbsync " emacs-version)))
    (pcase account
      ("All" (concat command "-a"))
      (else (concat command else)))))

(defun mu4easy-update-mail-and-index ()
  "Run a mu4e update; if prefix, focus on a specific account."
  (interactive)
  (mu4e-kill-update-mail)
  (if current-prefix-arg
      (let ((mu4e-get-mail-command (mu4easy-update-custom-account)))
        (mu4e-update-mail-and-index nil))
    (mu4e-update-mail-and-index nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trash without trashed flag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (alist-get 'trash mu4e-marks)
      '(:char ("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
              ;; Here's the main difference to the regular trash mark, no +T
              ;; before -N so the message is not marked as IMAP-deleted:
              :action (lambda (docid msg target)
                        (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consistent Refile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (alist-get 'refile mu4e-marks)
      '(:char ("r" . "▶")
              :prompt "refile"
              :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
              :action (lambda (docid msg target)
                        (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Respond in text-mode if prefix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sometimes I want to respond in text to HTML messages
;; e.g. when participating in github discussions using email
(defun mu4easy-org-msg-select-format (alternative)
  "Wrapping function to override email format (html/text).
Argument ALTERNATIVE passthrough agrument when advicing."
  (if current-prefix-arg '(text) alternative))

(advice-add 'org-msg-get-alternatives
            :filter-return #'mu4easy-org-msg-select-format)

;; Text Mode Signature

(defun mu4easy-customize-org-msg (orig-fun &rest args)
  "Fix for signature and greeting when email is text.
Argument ORIG-FUN function being adviced.
Optional argument ARGS ."
  (let ((res (apply orig-fun args)))
    (when (equal (cadr args) '(text))
      (setf (alist-get 'signature res)
            (replace-regexp-in-string "\\([\*/]\\)" ""
                                      org-msg-signature))
      (setf (alist-get 'greeting-fmt res) ""))
    res))

(advice-add 'org-msg-composition-parameters :around #'mu4easy-customize-org-msg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro for Contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmacro mu4easy-context (&key c-name maildir mail smtp
                                   (smtp-mail mail)
                                   (smtp-port 587)
                                   (smtp-type 'starttls)
                                   (sent-action 'sent)
                                   (name user-full-name)
                                   (sig mu4easy-signature))
  "Main macro for creating email accounts (contexts).
C-NAME context name, used in mu4e UI; first letter is going to be
    used as a shortcut.
MAILDIR mail dir under path/Mail/...
MAIL email address or alias.
SMTP address.
SMTP-MAIL email address for this account (not alias).
SMTP-TYPE default `starttls'.
SMTP-PORT default 587.
SENT-ACTION what to do after sending an email (copy to `sent' or delete);
    see README.
NAME name can be set per account.
SIG signature string; supports org formatting thanks to org-msg."
  (let
      ((inbox  (concat "/" maildir "/Inbox"))
       (sent   (concat "/" maildir "/Sent"))
       (trash  (concat "/" maildir "/Trash"))
       (refile (concat "/" maildir "/Archive"))
       (draft  (concat "/" maildir "/Drafts"))
       (spam   (concat "/" maildir "/Spam")))
    
    `(make-mu4e-context
      :name ,c-name
      :match-func (lambda (msg)
                    (when msg
                      (string-match-p (concat "^/" ,maildir "/")
                                      (mu4e-message-field msg :maildir))))
      :vars '((user-mail-address . ,mail)
              (user-full-name . ,name)
              (mu4e-sent-folder . ,sent)
              (mu4e-drafts-folder . ,draft)
              (mu4e-trash-folder . ,trash)
              (mu4e-refile-folder . ,refile)
              (mu4e-compose-signature . (concat ,sig))
              (mu4e-compose-format-flowed . t)
              (mu4e-sent-messages-behavior . ,sent-action)
              (smtpmail-smtp-user . ,smtp-mail)
              (smtpmail-starttls-credentials . ((,smtp ,smtp-port nil nil)))
              (smtpmail-auth-credentials . '((,smtp ,smtp-port ,smtp-mail nil)))
              (smtpmail-default-smtp-server . ,smtp)
              (smtpmail-smtp-server . ,smtp)
              (smtpmail-stream-type . ,smtp-type)
              (smtpmail-smtp-service . ,smtp-port)
              (smtpmail-debug-info . t)
              (smtpmail-debug-verbose . t)
              (org-msg-signature . ,sig)
              (mu4e-maildir-shortcuts .
                                      ((,inbox   . ?i)
                                       (,sent    . ?s)
                                       (,trash   . ?t)
                                       (,refile  . ?a)
                                       (,draft   . ?d)
                                       (,spam    . ?g)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mu4easy-mail-accounts mu4easy-account
      message-citation-line-function 'message-insert-formatted-citation-line
      message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it
      mu4e-attachment-dir (expand-file-name mu4easy-download-dir)
      mu4e-change-filenames-when-moving t
      mu4e-completing-read-function 'completing-read
      mu4e-compose-context-policy 'ask
      mu4e-compose-format-flowed t
      mu4e-compose-signature-auto-include nil
      mu4e-confirm-quit nil
      mu4e-context-policy 'pick-first
      mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version)
      mu4e-headers-auto-update t
      mu4e-headers-date-format "%d/%m/%Y %H:%M"
      mu4e-headers-include-related nil
      mu4e-headers-skip-duplicates t
      mu4e-index-cleanup t
      mu4e-index-lazy-check nil
      mu4e-maildir (expand-file-name mu4easy-maildir)
      mu4e-main-buffer-hide-personal-addresses t
      mu4e-main-buffer-name "*mu4e-main*"
      mu4e-mu-binary "/usr/local/bin/mu"
      mu4e-org-link-desc-func 'mu4easy-mail-link-description
      mu4e-sent-messages-behavior 'sent
      mu4e-update-interval 400
      mu4e-use-fancy-chars t
      mu4e-view-prefer-html t
      mu4e-view-show-addresses 't
      mu4e-view-show-images t
      smtpmail-smtp-service 587
      starttls-use-gnutls t
      message-citation-line-format "%N [%Y-%m-%d %a %H:%M] wrote:
"
      mu4easy-today-query "date:today..now AND NOT maildir:/Trash/ AND NOT maildir:/Spam/"
      mu4easy-trash-query "maildir:/Trash/"
      mu4easy-inbox-query "maildir:/Inbox/"
      mu4easy-unread-query "flag:new AND maildir:/Inbox/")

(setq mu4e-bookmarks
      `(( :name  "Unread"
          :query ,mu4easy-unread-query
          :key   ?u)
        ( :name  "Inbox"
          :query ,mu4easy-inbox-query
          :key   ?i)
        ( :name "Today"
          :query ,mu4easy-today-query
          :key   ?t)
        ( :name "Flagged"
          :query "flag:flagged"
          :key   ?f)
        ( :name "Tags"
          :query "tag://"
          :key   ?T)
        ( :name "Trash"
          :query ,mu4easy-trash-query
          :key ?x
          :hide-unread t)
        ( :name "Attachments"
          :query "mime:application/pdf or mime:image/jpg or mime:image/png"
          :key   ?a
          :hide-unread t)))

(setq mu4e-headers-fields
      '((:human-date   . 18)
        (:flags        . 6)
        (:maildir      . 16)
        (:from-or-to   . 22)
        (:mailing-list . 10)
        (:tags         . 10)
        (:subject      . 92)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail Identities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mu4e-contexts
      `(,(mu4easy-context
          :c-name  "Google"
          :maildir "Gmail"
          :mail    "a@gmail.com"
          :smtp    "smtp.gmail.com"
          :sent-action delete)

        ,(mu4easy-context
          :c-name  "1-GMX"
          :maildir "GMX"
          :mail    "a@gmx.com"
          :smtp    "mail.gmx.com")

        ,(mu4easy-context
          :c-name    "2-GMX-alias"
          :maildir   "GMX"
          :mail      "a.alias@gmx.com"
          :smtp      "mail.gmx.com"
          :smtp-mail "a@gmx.com")

        ,(mu4easy-context
          :c-name  "Apple"
          :maildir "Apple"
          :mail    "a@icloud.com"
          :smtp    "smtp.mail.me.com")

        ,(mu4easy-context
          :c-name  "3-Apple-alias"
          :maildir "Apple"
          :mail    "a@me.com"
          :smtp    "smtp.mail.me.com"
          :smtp-mail "a@icloud.com")

        ,(mu4easy-context
          :c-name    "Proton"
          :maildir   "Proton"
          :mail      "a@protonmail.com"
          :smtp      "127.0.0.1"
          :smtp-type ssl
          :smtp-port 999)

        ,(mu4easy-context
          :c-name    "4-Proton-alias"
          :maildir   "Proton"
          :mail      "a@pm.com"
          :smtp      "127.0.0.1"
          :smtp-mail "a@protonmail.com"
          :smtp-type ssl
          :smtp-port 999)))

(provide 'mu4easy)
;;; mu4easy.el ends here
