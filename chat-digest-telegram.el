;;; chat-digest-telegram.el --- Telegram chats for chat-digest, through telega -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Oleksii Sadovyi

;; Author: Oleksii Sadovyi <3742880+OSadovy@users.noreply.github.com>
;; Keywords: comm
;; Package-Requires: ((emacs "29.1") (telega "0.8"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Reads Telegram chats through telega's running session.
;;
;; Fetching history does not mark anything read.  Telegram reads a chat up
;; to a message, not message by message, so marking the last message of a
;; digest read is what approving it does.
;;
;; A message is opened in telega's preview mode.  TDLib marks messages read
;; only when they are viewed in an open chat, and preview mode closes the
;; chat, so reading around a message from a digest leaves the read position
;; alone.  Opening the chat from telega's root buffer as usual leaves preview
;; mode again.

;;; Code:

(require 'cl-lib)
(require 'chat-digest)
(require 'telega)

(defun chat-digest-telegram--chat (chat)
  (or (telega-chat-get (plist-get chat :id) 'offline)
      (user-error "Telegram chat %s is not known to telega" (plist-get chat :id))))

(defun chat-digest-telegram--handle (tchat)
  "Return the chat-digest chat for telega chat TCHAT."
  (list :source "telegram"
        :account (number-to-string telega--me-id)
        :id (plist-get tchat :id)
        :title (telega-tl-str tchat :title 'no-props)))

(defun chat-digest-telegram--chat-at-point ()
  (when-let ((tchat (or (bound-and-true-p telega-chatbuf--chat)
                        (and (derived-mode-p 'telega-root-mode)
                             (telega-chat-at (point))))))
    (chat-digest-telegram--handle tchat)))

(defun chat-digest-telegram--read-chat ()
  (unless (telega-server-live-p)
    (user-error "Telega is not running"))
  (chat-digest-telegram--handle (telega-completing-read-chat "Digest of chat: ")))

(defun chat-digest-telegram--read-position (chat)
  (plist-get (chat-digest-telegram--chat chat) :last_read_inbox_message_id))

;;;; Messages

(defun chat-digest-telegram--sender-name (sender tchat)
  "Return the name of message SENDER, and whether it is TCHAT itself."
  (pcase (plist-get sender :@type)
    ("messageSenderUser"
     (let ((user (telega-user-get (plist-get sender :user_id) 'locally)))
       (list (or (and user (telega-user-title user 'full-name 'no-badges))
                 (format "user %s" (plist-get sender :user_id)))
             nil)))
    ("messageSenderChat"
     (let* ((id (plist-get sender :chat_id))
            (chat (telega-chat-get id 'offline)))
       (list (or (and chat (telega-tl-str chat :title 'no-props))
                 (format "chat %s" id))
             (eql id (plist-get tchat :id)))))))

(defun chat-digest-telegram--forward (msg)
  (when-let ((origin (plist-get (plist-get msg :forward_info) :origin)))
    (pcase (plist-get origin :@type)
      ("messageOriginChannel"
       (list :kind 'channel
             :name (or (telega-tl-str (telega-chat-get (plist-get origin :chat_id)
                                                       'offline)
                                      :title 'no-props)
                       (plist-get origin :author_signature)
                       "a channel")))
      ("messageOriginChat"
       (list :kind 'chat
             :name (or (telega-tl-str (telega-chat-get (plist-get origin :sender_chat_id)
                                                       'offline)
                                      :title 'no-props)
                       "a chat")))
      ("messageOriginUser"
       (let ((user (telega-user-get (plist-get origin :sender_user_id) 'locally)))
         (list :kind 'user
               :name (or (and user (telega-user-title user 'full-name 'no-badges))
                         "someone"))))
      ("messageOriginHiddenUser"
       (list :kind 'user :name (plist-get origin :sender_name))))))

(defun chat-digest-telegram--entity-urls (fmt-text)
  "Return the URLs in formatted text FMT-TEXT.
Entity offsets count UTF-16 code units.  telega keeps the text from
TDLib with each astral character as its two surrogates, so the offsets
index the raw string directly and only the result needs desurrogating."
  (let ((text (plist-get fmt-text :text)))
    (delete-dups
     (cl-loop for e across (or (plist-get fmt-text :entities) [])
              for type = (plist-get e :type)
              for url = (pcase (plist-get type :@type)
                          ("textEntityTypeTextUrl" (plist-get type :url))
                          ("textEntityTypeUrl"
                           (let* ((start (plist-get e :offset))
                                  (end (+ start (plist-get e :length))))
                             (when (<= end (length text))
                               (telega--desurrogate-apply
                                (substring text start end) 'no-props)))))
              when url
              collect (if (string-match-p "\\`[a-z]+://" url) url
                        (concat "https://" url))))))

(defun chat-digest-telegram--content (content)
  "Return the text and formatted text of message CONTENT, or nil to skip it."
  (let* ((type (plist-get content :@type))
         (caption (plist-get content :caption))
         (label (lambda (what)
                  (concat "[" what "]"
                          (when-let ((c (telega-tl-str content :caption 'no-props)))
                            (concat " " c))))))
    (pcase type
      ("messageText" (list (or (telega-tl-str content :text 'no-props) "")
                           (plist-get content :text)))
      ("messagePhoto" (list (funcall label "photo") caption))
      ("messageVideo" (list (funcall label "video") caption))
      ("messageAnimation" (list (funcall label "GIF") caption))
      ("messageDocument"
       (list (funcall label
                      (format "file %s"
                              (or (telega-tl-str (plist-get content :document)
                                                 :file_name 'no-props)
                                  "")))
             caption))
      ("messageAudio" (list (funcall label "audio") caption))
      ("messageVoiceNote" (list (funcall label "voice message") caption))
      ("messageVideoNote" (list "[video message]" nil))
      ("messageSticker"
       (list (format "[sticker %s]"
                     (or (telega-tl-str (plist-get content :sticker) :emoji 'no-props)
                         ""))
             nil))
      ("messagePoll"
       (let ((poll (plist-get content :poll)))
         (list (format "[poll] %s: %s"
                       (telega-tl-str poll :question 'no-props)
                       (mapconcat (lambda (o) (telega-tl-str o :text 'no-props))
                                  (plist-get poll :options) "; "))
               nil)))
      ("messagePinMessage" (list "" nil))
      ;; Joins, leaves, title changes and the rest of the service messages.
      ((guard (string-prefix-p "messageChat" type)) nil)
      (_ (list (format "[%s]" (string-remove-prefix "message" type)) nil)))))

(defun chat-digest-telegram--message (msg tchat)
  "Return telega message MSG of TCHAT as a chat-digest message, or nil."
  (let* ((content (plist-get msg :content))
         (body (chat-digest-telegram--content content)))
    (when body
      (pcase-let* ((`(,text ,fmt-text) body)
                   (`(,sender ,as-chat)
                    (chat-digest-telegram--sender-name (plist-get msg :sender_id) tchat))
                   (preview (plist-get content :link_preview))
                   (reply-to (plist-get msg :reply_to))
                   (urls (and fmt-text (chat-digest-telegram--entity-urls fmt-text)))
                   (preview-url (plist-get preview :url))
                   ;; The preview's URL and the one in the text can differ by
                   ;; a trailing slash.
                   (same (lambda (a b) (equal (string-trim-right a "/")
                                              (string-trim-right b "/")))))
        (when (and preview-url
                   (not (cl-member preview-url urls :test same)))
          (setq urls (append urls (list preview-url))))
        (list :id (plist-get msg :id)
              :date (plist-get msg :date)
              :sender sender
              :as-chat as-chat
              :text text
              :reply-to (and (equal (plist-get reply-to :@type) "messageReplyToMessage")
                             (memql (plist-get reply-to :chat_id)
                                    (list 0 (plist-get tchat :id)))
                             (plist-get reply-to :message_id))
              :quote (telega-tl-str (plist-get reply-to :quote) :text 'no-props)
              :pinned (and (equal (plist-get content :@type) "messagePinMessage")
                           (plist-get content :message_id))
              :forward (chat-digest-telegram--forward msg)
              :reactions (cl-loop for r across (or (plist-get
                                                    (plist-get (plist-get msg :interaction_info)
                                                               :reactions)
                                                    :reactions)
                                                   [])
                                  sum (plist-get r :total_count))
              :links (mapcar
                      (lambda (url)
                        (if (and preview-url (funcall same url preview-url))
                            (list :url url
                                  :title (telega-tl-str preview :title 'no-props)
                                  :description (telega-tl-str preview :description 'no-props))
                          (list :url url)))
                      urls))))))

(defun chat-digest-telegram--messages (msgs tchat)
  (sort (delq nil (mapcar (lambda (m) (chat-digest-telegram--message m tchat))
                          (append msgs nil)))
        (lambda (a b) (< (plist-get a :id) (plist-get b :id)))))

;;;; Fetching

(defun chat-digest-telegram--history (tchat from newer limit callback)
  "Call CALLBACK with up to LIMIT messages of TCHAT on one side of FROM.
NEWER non-nil means the messages after FROM; otherwise FROM and those
before it.  TDLib may answer with fewer messages than asked for, so
this asks again from the farthest one until a page adds nothing."
  (let ((seen (make-hash-table)) (acc nil))
    (cl-labels
        ((page (cursor)
           ;; With a negative offset TDLib wants the limit to be at least as
           ;; large, so pages after FROM are always asked for in hundreds.
           (telega--getChatHistory tchat cursor (if newer -99 0)
               (if newer 100 (min 100 (max 1 limit))) nil
             (lambda (reply)
               (let ((fresh (cl-remove-if
                             (lambda (m)
                               (let ((id (plist-get m :id)))
                                 (or (gethash id seen)
                                     (if newer (<= id from) (> id from)))))
                             (append (plist-get reply :messages) nil))))
                 (dolist (m fresh) (puthash (plist-get m :id) t seen))
                 (setq acc (nconc fresh acc))
                 (if (or (null fresh) (>= (hash-table-count seen) limit))
                     (funcall callback acc)
                   (page (apply (if newer #'max #'min)
                                (mapcar (lambda (m) (plist-get m :id)) fresh)))))))))
      (page from))))

(defun chat-digest-telegram--fetch-after (chat id limit callback)
  (let ((tchat (chat-digest-telegram--chat chat)))
    (chat-digest-telegram--history
     tchat id t limit
     (lambda (msgs)
       (funcall callback
                (seq-take (chat-digest-telegram--messages msgs tchat) limit))))))

(defun chat-digest-telegram--fetch-before (chat id limit callback)
  (let ((tchat (chat-digest-telegram--chat chat)))
    (chat-digest-telegram--history
     tchat id nil limit
     (lambda (msgs)
       (funcall callback
                (last (chat-digest-telegram--messages msgs tchat) limit))))))

(defun chat-digest-telegram--fetch-ids (chat ids callback)
  (let ((tchat (chat-digest-telegram--chat chat)))
    (telega--getMessages (plist-get tchat :id) ids
      (lambda (msgs)
        (funcall callback (chat-digest-telegram--messages msgs tchat))))))

(defun chat-digest-telegram--mark-read (chat id callback)
  (telega--viewMessages (chat-digest-telegram--chat chat) (list (list :id id))
    :source '(:@type "messageSourceOther")
    :force t
    :callback (lambda (reply)
                (funcall callback (not (telega--tl-error-p reply))))))

(defun chat-digest-telegram--link-preview (url callback)
  (telega--getLinkPreview (list :@type "formattedText" :text url :entities [])
                          nil
    (lambda (preview)
      (funcall callback
               (unless (telega--tl-error-p preview)
                 (list :title (telega-tl-str preview :title 'no-props)
                       :description (telega-tl-str preview :description 'no-props)))))))

;;;; Opening

(defun chat-digest-telegram--open-message (account chat-id message-id)
  (unless (telega-server-live-p)
    (user-error "Telega is not running"))
  (unless (equal account (number-to-string telega--me-id))
    (user-error "This digest belongs to another Telegram account, %s" account))
  (let* ((tchat (telega-chat-get (string-to-number chat-id)))
         (buffer (telega-chatbuf--get-create tchat :no-history-load)))
    ;; Preview mode goes on before the buffer is displayed: creating it opens
    ;; the chat, and nothing may be viewed until preview mode closes it again.
    (with-current-buffer buffer
      (unless telega-chat-preview-mode
        (telega-chat-preview-mode 1)))
    (with-current-buffer (telega-chat--pop-to-buffer tchat :no-history-load)
      (telega-chatbuf--goto-msg (string-to-number message-id) 'highlight))))

(chat-digest-register-source
 (chat-digest-source-create
  :name "telegram"
  :chat-at-point #'chat-digest-telegram--chat-at-point
  :read-chat #'chat-digest-telegram--read-chat
  :read-position #'chat-digest-telegram--read-position
  :fetch-after #'chat-digest-telegram--fetch-after
  :fetch-before #'chat-digest-telegram--fetch-before
  :fetch-ids #'chat-digest-telegram--fetch-ids
  :mark-read #'chat-digest-telegram--mark-read
  :open-message #'chat-digest-telegram--open-message
  :link-preview #'chat-digest-telegram--link-preview))

(provide 'chat-digest-telegram)
;;; chat-digest-telegram.el ends here
