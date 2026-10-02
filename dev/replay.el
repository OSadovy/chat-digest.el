;;; replay.el --- Run digests over a recorded chat -*- lexical-binding: t; -*-

;;; Commentary:

;; A source serving messages recorded to a JSON file, for trying the prompt
;; and the whole digest pipeline without a live chat:
;;
;;   emacs -Q --batch -L . -l dev/replay.el -f chat-digest-replay-batch \
;;     RECORDING STORE [DIGESTS]
;;
;; RECORDING holds {"title", "position", "messages": [...]}, each message
;; with the fields of a chat-digest message spelled in snake case.  STORE
;; becomes `chat-digest-directory'.  DIGESTS digests are made one after the
;; other, each approved, so the registry carries from one to the next; each
;; is copied to STORE/replay-N.org.  Marking read moves only the recording's
;; own position.

;;; Code:

(require 'chat-digest)

(defvar chat-digest-replay--title nil)
(defvar chat-digest-replay--position nil)
(defvar chat-digest-replay--messages nil)

(defun chat-digest-replay--message (m)
  (list :id (gethash "id" m) :date (gethash "date" m)
        :sender (gethash "sender" m) :text (gethash "text" m)
        :reply-to (gethash "reply_to" m)
        :quote (gethash "quote" m)
        :as-chat (gethash "as_chat" m)
        :reactions (gethash "reactions" m)
        :forward (when-let ((f (gethash "forward" m)))
                   (list :kind (intern (gethash "kind" f)) :name (gethash "name" f)))
        :links (mapcar (lambda (l)
                         (list :url (gethash "url" l) :title (gethash "title" l)
                               :description (gethash "description" l)))
                       (gethash "links" m))))

(defun chat-digest-replay-load (file)
  (let ((rec (with-temp-buffer
               (insert-file-contents file)
               (json-parse-buffer :object-type 'hash-table :array-type 'list
                                  :null-object nil :false-object nil))))
    (setq chat-digest-replay--title (gethash "title" rec)
          chat-digest-replay--position (gethash "position" rec)
          chat-digest-replay--messages
          (mapcar #'chat-digest-replay--message (gethash "messages" rec)))))

(chat-digest-register-source
 (chat-digest-source-create
  :name "replay"
  :read-position (lambda (_chat) chat-digest-replay--position)
  :fetch-after
  (lambda (_chat id limit callback)
    (funcall callback (seq-take (cl-remove-if-not
                                 (lambda (m) (> (plist-get m :id) id))
                                 chat-digest-replay--messages)
                                limit)))
  :fetch-before
  (lambda (_chat id limit callback)
    (funcall callback (last (cl-remove-if-not
                             (lambda (m) (<= (plist-get m :id) id))
                             chat-digest-replay--messages)
                            limit)))
  :fetch-ids
  (lambda (_chat ids callback)
    (funcall callback (cl-remove-if-not
                       (lambda (m) (memql (plist-get m :id) ids))
                       chat-digest-replay--messages)))
  :mark-read
  (lambda (_chat id callback)
    (setq chat-digest-replay--position (max id chat-digest-replay--position))
    (funcall callback t))
  :open-message #'ignore))

(defun chat-digest-replay-batch ()
  (pcase-let ((`(,recording ,store ,n) command-line-args-left))
    (setq command-line-args-left nil
          chat-digest-directory (expand-file-name store)
          chat-digest-continue nil)
    (chat-digest-replay-load recording)
    (let ((chat (list :source "replay" :account "local" :id "chat"
                      :title chat-digest-replay--title)))
      (dotimes (i (string-to-number (or n "1")))
        (let ((started (float-time)))
          (chat-digest--start chat)
          (let (job)
            (while (and (setq job (gethash (chat-digest--key chat) chat-digest--jobs))
                        (not (eq (chat-digest--job-status job) 'ready)))
              (accept-process-output nil 0.5))
            (unless job (error "Digest %d failed" (1+ i)))
            (message "Digest %d: %d messages, %d threads, %.0f seconds"
                     (1+ i) (length (chat-digest--job-chunk job))
                     (length (plist-get (chat-digest--job-digest job) :threads))
                     (- (float-time) started))
            (with-current-buffer (chat-digest--job-buffer job)
              (write-region nil nil (expand-file-name (format "replay-%d.org" (1+ i))
                                                      chat-digest-directory)))
            (chat-digest--approve job)
            (while (gethash (chat-digest--key chat) chat-digest--jobs)
              (accept-process-output nil 0.1))))))))

;;; replay.el ends here
