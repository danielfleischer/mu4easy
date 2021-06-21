;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :bind (("C-c u" . mu4e)
         :map mu4e-main-mode-map
         ("x" . bury-buffer))
  :defer 6
  :config
  (setq mail-user-agent 'mu4e-user-agent)

  (use-package smtpmail)

  ;; Optional
  (use-package org-mime :ensure t)

  ;; Optional
  (use-package mu4e-contrib
    :defer 2
    :bind (:map mu4e-headers-mode-map
                ("M" . mu4e-headers-mark-all)
                ("N" . mu4e-headers-mark-all-unread-read)))

  ;; Optional
  (use-package mu4e-goodies-tags
    :quelpa (mu4e-goodies-tags :fetcher github
                               :repo "panjie/mu4e-goodies"
                               :files ("mu4e-goodies-tags.el")))

  ;; Optional
  (use-package helm-mu
    :ensure t
    :bind
    (("C-c h h c" . 'helm-mu-contacts)
     (:map mu4e-main-mode-map ("s" . 'helm-mu))
     (:map mu4e-headers-mode-map ("s" . 'helm-mu))
     (:map mu4e-view-mode-map ("s" . 'helm-mu)))
    :config
    (setq helm-mu-append-implicit-wildcard t
          helm-mu-gnu-sed-program "gsed"))

  ;; Optional
  (use-package org-msg
    :ensure t
    :config 
    (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:imagemagick"
	      org-msg-startup "hidestars indent inlineimages"
	      org-msg-default-alternatives '((new	    	. (text html))
				                         (reply-to-html	. (text html))
				                         (reply-to-text	. (text)))
	      org-msg-convert-citation t)
    (org-msg-mode))

  (defun df/mail-link-description (msg)
    (let ((subject (or (plist-get msg :subject)
                       "No subject"))
          (date (or (format-time-string mu4e-headers-date-format
                                        (mu4e-msg-field msg :date))
                    "No date"))
          (to-from (mu4e~headers-from-or-to msg)))
      (message "%s: %s (%s)" to-from subject date)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Hooks
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (set-fill-column 100)
              (turn-on-auto-fill)
              (paragraph-indent-minor-mode)
              (electric-indent-local-mode -1)
              (turn-on-flyspell)))
  (add-to-list 'mu4e-view-actions
               '("View In Browser" . mu4e-action-view-in-browser) t)

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

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
                          (mu4e~proc-move docid
                                          (mu4e~mark-check-target target) "+S-u-N"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Refile-dwim       depends on provider
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq df/refile-dwim
        '(:char ("r" . "▶")
          :prompt "refile"
          :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
          :action (lambda (docid msg target)
                    (let ((maildir (mu4e-message-field msg :maildir)))
                      (if (string-match-p "Google\\|Gmail" maildir)
                          (mu4e~proc-remove docid)
                        (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))))))
  (setf (alist-get 'refile mu4e-marks) df/refile-dwim)
        
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Macro for Contexts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (cl-defmacro df/mu4e-context (&key c-name maildir mail smtp
                                     (smtp-mail mail)
                                     (smtp-port 587)
                                     (smtp-type 'starttls)
                                     (sent-action 'sent)
                                     (name "Daniel Fleischer")
                                     (sig "\n--\nDaniel Fleischer"))
    (let
        ((inbox    (concat "/" maildir "/Inbox"))  
         (sent	   (concat "/" maildir "/Sent"))
	 (trash	   (concat "/" maildir "/Trash"))
	 (refile   (concat "/" maildir "/Archive"))
	 (draft	   (concat "/" maildir "/Drafts")))
      
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
                (mu4e-sent-messages-behavior . ,sent-action)
	        (smtpmail-smtp-user . ,smtp-mail)
	        (smtpmail-starttls-credentials . ((,smtp ,smtp-port nil nil)))
                (smtpmail-auth-credentials . '((,smtp ,smtp-port ,smtp-mail nil)))
	        (smtpmail-default-smtp-server . ,smtp)
	        (smtpmail-smtp-server . ,smtp)
                (smtpmail-stream-type . ,smtp-type)
	        (smtpmail-smtp-service . ,smtp-port)
                (org-msg-signature . ,sig)
	        (mu4e-maildir-shortcuts . 
                                            ((,inbox   . ?i)
				             (,sent    . ?s)
				             (,trash   . ?t)
				             (,refile  . ?a)
				             (,draft   . ?d)))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Variables
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq df-mail-accounts '("Gmail" "GMX" "Proton" "Apple") 
        message-citation-line-format "On %a, %b %d %Y, %N wrote:"
        message-citation-line-function 'message-insert-formatted-citation-line
        message-kill-buffer-on-exit t
        message-send-mail-function 'smtpmail-send-it
        mu4e-attachment-dir (expand-file-name "~/Downloads")
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
        mu4e-index-cleanup nil
        mu4e-index-lazy-check t
        mu4e-maildir (expand-file-name "~/Mail")       ;; IMPORTANT
        mu4e-main-buffer-hide-personal-addresses t
        mu4e-main-buffer-name "*mu4e-main*"
        mu4e-mu-binary "/usr/local/bin/mu"
        mu4e-org-link-desc-func 'df/mail-link-description
        mu4e-sent-messages-behavior 'sent
        mu4e-update-interval 600
        mu4e-use-fancy-chars nil
        mu4e-view-prefer-html t
        mu4e-view-show-addresses 't
        mu4e-view-show-images t
        smtpmail-smtp-service 587
        starttls-use-gnutls t

        df/inbox-query (mapconcat
                        (lambda (d) (format "m:/%s/Inbox" d))
                          df-mail-accounts " OR ")
        df/today-query (concat "date:today..now AND NOT "
                          (mapconcat
                           (lambda (d) (format "m:/%s/Trash" d))
                           df-mail-accounts " AND NOT "))
        df/trash-query (mapconcat
                        (lambda (d) (format "m:/%s/Trash" d))
                        df-mail-accounts " OR ")
        df/unread-query (concat "flag:unread AND NOT (" df/trash-query ")"))
  

  (setq mu4e-bookmarks  `(( :name  "Unread"
                            :query ,df/unread-query
                            :key   ?u)
                          ( :name  "Inbox"
                            :query ,df/inbox-query
                            :key   ?i)
                          ( :name "Today"
                            :query ,df/today-query
                            :key   ?t)
                          ( :name "Flagged"
                            :query "flag:flagged"
                            :key   ?f)
                          ( :name "Tags"
                            :query "tag://"
                            :key   ?T)
                          ( :name "Trash"
                            :query ,df/trash-query
                            :key ?x
                            :hide-unread t)
                          ( :name "Attachments"
                            :query "mime:application/pdf or mime:image/jpg or mime:image/png"
                            :key   ?a
                            :hide-unread t)))

  (setq mu4e-headers-fields '((:human-date . 18)
                              (:flags      . 6)
                              (:maildir    . 16)
                              (:from-or-to . 26)
                              (:subject    . 90)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mail Identities
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq mu4e-contexts  `(,(df/mu4e-context
                           :c-name  "Google"
                           :maildir "Gmail"
                           :mail    "a@gmail.com"
                           :smtp    "smtp.gmail.com"
                           :sent-action delete)

                         ,(df/mu4e-context
                           :c-name  "1-GMX"
                           :maildir "GMX"
                           :mail    "a@gmx.com"
                           :smtp    "mail.gmx.com")

                         ,(df/mu4e-context
                           :c-name    "2-GMX-alias"
                           :maildir   "GMX"
                           :mail      "a.alias@gmx.com"
                           :smtp      "mail.gmx.com"
                           :smtp-mail "a@gmx.com")

                         ,(df/mu4e-context
                           :c-name  "Apple"
                           :maildir "Apple"
                           :mail    "a@icloud.com"
                           :smtp    "smtp.mail.me.com")

                         ,(df/mu4e-context
                           :c-name  "3-Apple-alias"
                           :maildir "Apple"
                           :mail    "a@me.com"
                           :smtp    "smtp.mail.me.com"
                           :smtp-mail "a@icloud.com")

                         ,(df/mu4e-context
                           :c-name    "Proton"
                           :maildir   "Proton"
                           :mail      "a@protonmail.com"
                           :smtp      "127.0.0.1"
                           :smtp-type ssl
                           :smtp-port 999)

                         ,(df/mu4e-context
                           :c-name    "4-Proton-alias"
                           :maildir   "Proton"
                           :mail      "a@pm.com"
                           :smtp      "127.0.0.1"
                           :smtp-mail "a@protonmail.com"
                           :smtp-type ssl
                           :smtp-port 999))))

