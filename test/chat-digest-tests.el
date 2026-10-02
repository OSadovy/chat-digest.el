;;; chat-digest-tests.el --- Tests for chat-digest -*- lexical-binding: t; -*-

;;; Commentary:

;; What is tested is our own logic: where a digest is cut, what context it
;; gets, how the registry changes on approval, and how a model answer is
;; checked before anything is rendered or recorded from it.  The model, TDLib,
;; fxtwitter and Org rendering are not; dev/replay.el runs those end to end.

;;; Code:

(require 'ert)
(require 'chat-digest)

(defun chat-digest-test--messages (&rest dates)
  "Return messages numbered from 1, sent at DATES."
  (let ((n 0))
    (mapcar (lambda (d) (list :id (cl-incf n) :date d :sender "s" :text "t"))
            dates)))

(ert-deftest chat-digest-test-split-chunk ()
  (let ((ms (chat-digest-test--messages 0 10 20 2000 2010 2020 2030 2040)))
    ;; Few enough: all of them.
    (should (equal (chat-digest--split-chunk ms 8 1200) ms))
    ;; Too many: cut at the lull after the third.
    (should (equal (mapcar (lambda (m) (plist-get m :id))
                           (chat-digest--split-chunk ms 6 1200))
                   '(1 2 3)))
    ;; A lull that would keep less than half is ignored.
    (should (= (length (chat-digest--split-chunk ms 7 1200)) 7))
    ;; No lull: cut at the limit.
    (should (= (length (chat-digest--split-chunk ms 5 100000)) 5))))

(ert-deftest chat-digest-test-lead-in ()
  (let* ((older (chat-digest-test--messages 0 10 3000 3010 3020))
         (first (list :id 9 :date 3030)))
    (should (equal (mapcar (lambda (m) (plist-get m :id))
                           (chat-digest--lead-in older first 1200 100))
                   '(3 4 5)))
    (should (= (length (chat-digest--lead-in older first 1200 2)) 2))
    ;; The read position itself ended a conversation.
    (should-not (chat-digest--lead-in older (list :id 9 :date 9000) 1200 100))))

(defun chat-digest-test--state ()
  (let ((state (make-hash-table :test #'equal))
        (threads (make-hash-table :test #'equal))
        (messages (make-hash-table :test #'equal))
        (t1 (make-hash-table :test #'equal)))
    (puthash "title" "Old" t1)
    (puthash "summary" "Before." t1)
    (puthash "last_active" 100 t1)
    (puthash "digests" ["a.org"] t1)
    (puthash "t1" t1 threads)
    (puthash "50" "t1" messages)
    (puthash "next_thread" 2 state)
    (puthash "threads" threads state)
    (puthash "messages" messages state)
    state))

(ert-deftest chat-digest-test-commit ()
  (let ((state (chat-digest-test--state))
        (m1 (list :id 60 :date 500))
        (m2 (list :id 61 :date 600))
        (m3 (list :id 62 :date 700)))
    (chat-digest--commit
     state
     (list :threads
           (list (list :thread "t1" :title "Renamed" :summary "Now."
                       :messages (list m1))
                 (list :thread nil :title "Fresh" :summary "New."
                       :messages (list m2))
                 ;; An id the registry never had is a new thread too.
                 (list :thread "t9" :title "Made up" :summary "x"
                       :messages (list m3))))
     "b.org")
    (let ((threads (gethash "threads" state))
          (messages (gethash "messages" state)))
      (should (equal (gethash "title" (gethash "t1" threads)) "Renamed"))
      (should (equal (gethash "digests" (gethash "t1" threads)) ["a.org" "b.org"]))
      (should (= (gethash "last_active" (gethash "t1" threads)) 500))
      (should (equal (gethash "title" (gethash "t2" threads)) "Fresh"))
      (should (equal (gethash "title" (gethash "t3" threads)) "Made up"))
      (should-not (gethash "t9" threads))
      (should (= (gethash "next_thread" state) 4))
      (should (equal (mapcar (lambda (id) (gethash id messages)) '("50" "60" "61" "62"))
                     '("t1" "t1" "t2" "t3"))))))

(ert-deftest chat-digest-test-thread-of ()
  (let* ((state (chat-digest-test--state))
         (by-id (make-hash-table :test #'equal))
         (held (list :id 55 :reply-to 50))
         (new (list :id 70 :reply-to 55))
         (to-unheld (list :id 71 :reply-to 50))
         (loose (list :id 72)))
    (puthash 55 held by-id)
    (should (equal (chat-digest--thread-of new by-id state) "t1"))
    (should (equal (chat-digest--thread-of to-unheld by-id state) "t1"))
    (should-not (chat-digest--thread-of loose by-id state))))

(ert-deftest chat-digest-test-parse ()
  (let* ((context (list :id 1 :date 0))
         (a (list :id 2 :date 10))
         (b (list :id 3 :date 20))
         (refs (make-hash-table :test #'equal))
         (answer
          (list :overview "o"
                :threads
                (list (list :thread "new" :title "T" :headline "H"
                            :importance "high" :summary "S"
                            ;; m1 is context, m9 does not exist.
                            :messages '("m1" "m2" "m9")
                            :points (list (list :text "p" :messages '("m1" "m3")))
                            :links (list (list :url "https://a.example" :about "x"
                                               :message "m2")
                                         (list :url "https://invented.example"
                                               :about "y" :message "m2")))
                      ;; Only context: not a thread of this digest.
                      (list :thread "t1" :title "C" :headline "H" :importance "low"
                            :summary "S" :messages '("m1") :points nil :links nil))))
         digest)
    (puthash "m1" context refs)
    (puthash "m2" a refs)
    (puthash "m3" b refs)
    (setq digest (chat-digest--parse answer refs (list a b) '("https://a.example")))
    (should (= (length (plist-get digest :threads)) 1))
    (let ((th (car (plist-get digest :threads))))
      (should-not (plist-get th :thread))
      (should (eq (plist-get th :importance) 'high))
      (should (equal (plist-get th :messages) (list a)))
      ;; Points may cite context.
      (should (equal (plist-get (car (plist-get th :points)) :messages)
                     (list context b)))
      (should (equal (mapcar (lambda (l) (plist-get l :url)) (plist-get th :links))
                     '("https://a.example"))))))

(ert-deftest chat-digest-test-segments ()
  ;; Links to a known message or URL become Org links; others, plain words.
  (let ((refs (make-hash-table :test #'equal))
        (chat (list :source "s" :account "a" :id "c")))
    (puthash "m7" (list :id 7 :date 0 :sender "Bob | Team") refs)
    (should (equal (chat-digest--render-segments
                    chat
                    (chat-digest--segments
                     "Bob says [it is free](m7), see [the post](https://a.example); \
[made up](m99) and [elsewhere](https://b.example)."
                     refs '("https://a.example")))
                   "Bob says [[chat-digest:s/a/c/7][it is free]], see \
[[https://a.example][the post]]; made up and elsewhere."))
    ;; Without words of its own, a message link reads its sender and time.
    (should (string-match-p "\\[Bob [0-9][0-9]:[0-9][0-9]\\]\\]\\'"
                            (chat-digest--message-link chat (gethash "m7" refs))))))

(ert-deftest chat-digest-test-transcript ()
  (let* ((day 1790380800)               ; a midnight, local time aside
         (a (list :id 1 :date (+ day 36000) :sender "Ann" :text "Is the RPC\nfree?"))
         (b (list :id 2 :date (+ day 36060) :sender "Bob" :text "Yes" :reply-to 1))
         (c (list :id 3 :date (+ day 39600) :sender "Cy" :text "Which?" :reply-to 1
                  :quote "RPC"))
         (d (list :id 4 :date (+ day 39660) :sender "Di" :text "?" :reply-to 99))
         (names (make-hash-table :test #'equal))
         (by-id (make-hash-table :test #'equal)))
    (dolist (m (list a b c d))
      (puthash (plist-get m :id) (format "m%d" (plist-get m :id)) names)
      (puthash (plist-get m :id) m by-id))
    (let ((lines (split-string (chat-digest--transcript (list a b c d) names by-id)
                               "\n")))
      (should (string-prefix-p "== " (nth 0 lines)))
      (should (equal (nth 3 lines) "m2 Bob, replying to m1 (Ann: «Is the RPC free?»): Yes"))
      (should (equal (nth 4 lines) "… 59 minutes pass …"))
      ;; A quote stands in for the start of the message it quotes.
      (should (equal (nth 5 lines) "m3 Cy, replying to m1 (Ann: «RPC»): Which?"))
      (should (equal (nth 6 lines) "m4 Di, replying to a message not shown: ?")))))

(ert-deftest chat-digest-test-context ()
  ;; A reply chain five deep, whose first message answers, without replying,
  ;; the message two minutes before it; and an older message past a lull.
  (let* ((held (list (list :id 0 :date 0)
                     (list :id 1 :date 6000)
                     (list :id 2 :date 6120)
                     (list :id 3 :date 6600 :reply-to 2)
                     (list :id 4 :date 7000 :reply-to 3)
                     (list :id 5 :date 7400 :reply-to 4)))
         (source (chat-digest-source-create
                  :fetch-ids (lambda (_chat ids cb)
                               (funcall cb (cl-remove-if-not
                                            (lambda (m) (memql (plist-get m :id) ids))
                                            held)))
                  :fetch-before (lambda (_chat id limit cb)
                                  (funcall cb (last (cl-remove-if-not
                                                     (lambda (m) (<= (plist-get m :id) id))
                                                     held)
                                                    limit)))))
         (job (make-chat-digest--job
               :source source
               :chunk (list (list :id 10 :date 20000 :reply-to 5))))
         context)
    (cl-letf (((symbol-function 'chat-digest--enrich-and-ask)
               (lambda (_job c) (setq context c))))
      (chat-digest--gather-parents job nil nil chat-digest--reply-hops))
    (should (equal (mapcar (lambda (m) (plist-get m :id)) context) '(1 2 3 4 5)))))

(when (require 'telega nil t)
  (require 'chat-digest-telegram)

  (ert-deftest chat-digest-test-telegram-entity-urls ()
    ;; telega holds an astral character as its two UTF-16 surrogates, shown
    ;; through a `telega-display' property; TDLib's offsets count them both.
    (let* ((diver (propertize (string #xD83E #xDD3F)
                              'telega-display "🤿" 'telega-emoji-p t))
           (text (concat diver " see https://x.com/a/status/1 and example.org"))
           (fmt (list :@type "formattedText" :text text
                      :entities
                      (vector
                       (list :offset 7 :length 24
                             :type (list :@type "textEntityTypeUrl"))
                       (list :offset 36 :length 11
                             :type (list :@type "textEntityTypeUrl"))
                       (list :offset 3 :length 3
                             :type (list :@type "textEntityTypeTextUrl"
                                         :url "https://hidden.example/"))))))
      (should (equal (chat-digest-telegram--entity-urls fmt)
                     '("https://x.com/a/status/1" "https://example.org"
                       "https://hidden.example/"))))))

;;; chat-digest-tests.el ends here
