;;; chat-digest-gptel.el --- Ask a gptel backend for chat digests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Oleksii Sadovyi

;; Author: Oleksii Sadovyi <3742880+OSadovy@users.noreply.github.com>
;; Keywords: comm
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Sends the request for a digest through one of gptel's backends, with
;; tools switched off: the messages are written by strangers, and the model
;; reading them should be able to do nothing but answer.  Loaded when
;; `chat-digest-model' names gptel.

;;; Code:

(require 'gptel)

(defun chat-digest-gptel-run (backend model effort system prompt schema callback)
  "Ask MODEL on the gptel backend named BACKEND for an answer to PROMPT.
SYSTEM is the system prompt and SCHEMA the JSON schema the answer
follows.  EFFORT, unless nil, is sent as OpenAI's reasoning effort.
CALLBACK receives the answer as a plist, or a string saying what went
wrong.  Returns a function that cancels the request."
  (let ((b (or (gptel-get-backend backend)
               (user-error "No gptel backend called %s" backend))))
    ;; gptel quietly sends the backend's first model in place of one the
    ;; backend does not list.
    (unless (memq (intern model) (gptel-backend-models b))
      (user-error "gptel's %s backend does not list %s" backend model)))
  (let ((buffer (generate-new-buffer " *chat-digest-gptel*"))
        (spec (list :backend backend :model (intern model)
                    :use-tools nil :stream t
                    :request-params (and effort `(:reasoning (:effort ,effort)))))
        (chunks nil))
    (with-current-buffer buffer
      (gptel-with-preset spec
        (gptel-request prompt
          :system system :schema schema :stream t :buffer buffer
          :callback
          (lambda (response info)
            (cond ((stringp response) (push response chunks))
                  ((eq response t)
                   (kill-buffer buffer)
                   (funcall callback
                            (condition-case nil
                                (json-parse-string (apply #'concat (nreverse chunks))
                                                   :object-type 'plist
                                                   :array-type 'list
                                                   :null-object nil
                                                   :false-object nil)
                              (error "The answer was not the JSON asked for"))))
                  ((null response)
                   (kill-buffer buffer)
                   (funcall callback
                            (format "%s: %s" backend
                                    (or (plist-get info :error)
                                        (plist-get info :status))))))))))
    (lambda ()
      (when (buffer-live-p buffer)
        (gptel-abort buffer)
        (kill-buffer buffer)))))

(provide 'chat-digest-gptel)
;;; chat-digest-gptel.el ends here
