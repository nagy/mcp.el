;;; mcp-transport.el --- MCP transports              -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools
;; URL: https://github.com/lizqwerscott/mcp.el

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

;; Transport layer for MCP connections: the connection classes, the
;; JSON-RPC send method for stdio and HTTP, the stdio framing filter,
;; and the legacy HTTP+SSE stream setup.

;;; Code:

(require 'jsonrpc)
(require 'cl-lib)
(require 'url)

(defclass mcp-process-connection (jsonrpc-process-connection)
  ((connection-type
    :initarg :connection-type
    :accessor mcp--connection-type)
   (-status
    :initform 'init
    :accessor mcp--status)
   (-capabilities
    :initform nil
    :accessor mcp--capabilities)
   (-serverinfo
    :initform nil
    :accessor mcp--server-info)
   (-prompts
    :initform nil
    :accessor mcp--prompts)
   (-tools
    :initform nil
    :accessor mcp--tools)
   (-resources
    :initform nil
    :accessor mcp--resources)
   (-template-resources
    :initform nil
    :accessor mcp--template-resources)
   (-initial-callback
    :initarg :initial-callback
    :accessor mcp--initial-callback)
   (-prompts-callback
    :initarg :prompts-callback
    :accessor mcp--prompts-callback)
   (-tools-callback
    :initarg :tools-callback
    :accessor mcp--tools-callback)
   (-resources-callback
    :initarg :resources-callback
    :accessor mcp--resources-callback)
   (-resources-templates-callback
    :initarg :resources-templates-callback
    :accessor mcp--resources-templates-callback)
   (-error-callback
    :initarg :error-callback
    :accessor mcp--error-callback)
   (-roots
    :initarg :roots
    :initform nil
    :accessor mcp--roots)
   (-timeout
    :initarg :timeout
    :initform nil
    :accessor mcp--timeout
    :documentation "Timeout in seconds for jsonrpc async requests.
When nil, uses `jsonrpc-default-request-timeout'."))
  :documentation "A MCP connection over an Emacs process.")

(defclass mcp-http-process-connection (mcp-process-connection)
  ((-host
    :initarg :host
    :accessor mcp--host)
   (-port
    :initarg :port
    :accessor mcp--port)
   (-path
    :initarg :path
    :accessor mcp--path)
   (-tls
    :initarg :tls
    :accessor mcp--tls)
   (-token
    :initarg :token
    :accessor mcp--token)
   (-headers
    :initarg :headers
    :initform nil
    :accessor mcp--headers)
   (-sse
    :initform nil
    :accessor mcp--sse)
   (-endpoint
    :initform nil
    :accessor mcp--endpoint)
   (-session-id
    :initform nil
    :accessor mcp--session-id)
   (-running
    :initform t
    :accessor mcp--running))
  :documentation "A sse MCP connection over an Emacs process.")

(defclass mcp-stdio-process-connection (mcp-process-connection)
  ()
  :documentation "A stdio MCP connection over an Emacs process.")

(cl-defmethod initialize-instance :after ((_ mcp-process-connection) slots)
  "Init mcp process connection."
  (cl-destructuring-bind (&key ((:process proc)) &allow-other-keys) slots
    (set-process-filter proc #'mcp--process-filter)))

(cl-defmethod initialize-instance :around ((conn mcp-http-process-connection) slots)
  "Init mcp process connection."
  (shared-initialize conn slots)
  (setf (jsonrpc--process conn) nil))

(cl-defmethod jsonrpc-running-p ((conn mcp-http-process-connection))
  "Return non-nil if JSONRPC connection CONN is running."
  (mcp--running conn))

(cl-defmethod jsonrpc-shutdown :after ((conn mcp-http-process-connection))
  "Return non-nil if JSONRPC connection CONN is running."
  (setf (mcp--running conn) nil))

(defun mcp--resolve-value (value)
  "Resolve VALUE's value.
If it is function - call it.
If it is a variable - return it's value
Otherwise returns value itself."
  (cond
   ((functionp value) (funcall value))
   ((and (symbolp value) (boundp value)) (symbol-value value))
   (value)))

(defun mcp--parse-http-header (headers)
  "Parse HTTP response headers into a plist.

HEADERS is a string containing the raw HTTP response headers.
Returns a plist where each header field is a keyword (e.g. :content-type)
with its corresponding value.

Example:
  (mcp--parse-http-header \"Content-Type: text/html\\r\\nServer: nginx\\r\\n\")
  => (:content-type \"text/html\" :server \"nginx\")"
  (when-let* ((header-lines (split-string headers "\n"))
              (status-line (car header-lines))
              (res (split-string status-line " "))
              (status-code (elt res 1)))
    (plist-put (apply #'append
                      (mapcar (lambda (line)
                                (when-let* ((line (string-trim line))
                                            (search-sep-pos (string-search ":" line)))
                                  (list (intern
                                         (concat ":"
                                                 (downcase
                                                  (string-trim
                                                   (substring line 0 search-sep-pos)))))
                                        (string-trim
                                         (substring line
                                                    (+ search-sep-pos 1))))))
                              (cdr header-lines)))
               :response-code
               status-code)))

(cl-defmethod jsonrpc-connection-send ((connection mcp-process-connection)
                                       &rest args
                                       &key
                                       id
                                       method
                                       _params
                                       (_result nil result-supplied-p)
                                       error
                                       _partial)
  "Send JSON-RPC message to CONNECTION.
CONNECTION is an MCP process connection instance. ARGS is a plist
containing the message components:

METHOD - Method name (string, symbol or keyword)
PARAMS - Parameters for the method (optional)
ID     - Request ID (optional)
RESULT - Response result (for replies)
error   - Error object (for error replies)
partial - Partial response flag (optional)

For requests, both :method and :id should be provided.
For notifications, only :method is required.
For replies, either :_result or :error should be provided.

The message is sent differently based on connection type:
- SSE connections use HTTP POST requests
- Stdio connections write directly to the process"
  (when method
    ;; sanitize method into a string
    (setq args
          (plist-put args :method
                     (cond ((keywordp method) (substring (symbol-name method) 1))
                           ((symbolp method) (symbol-name method))
                           ((stringp method) method)
                           (t (error "[jsonrpc] invalid method %s" method))))))
  (let* ((kind (cond ((or result-supplied-p error) 'reply)
                     (id 'request)
                     (method 'notification)))
         (converted (jsonrpc-convert-to-endpoint connection args kind))
         (json (json-serialize converted
                               :false-object :json-false
                               :null-object :json-null)))
    (pcase (mcp--connection-type connection)
      ('http
       (let ((url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ,@(when-let* ((session-id (mcp--session-id connection)))
                    `(("Mcp-Session-Id" . ,session-id)))
                ,@(when-let* ((token (mcp--resolve-value (mcp--token connection))))
                    `(("Authorization" . ,(concat "Bearer " token))))
                ,@(when-let* ((headers (mcp--headers connection)))
                    `,headers)))
             (url-request-data (encode-coding-string
                                json
                                'utf-8))
             (url-mime-accept-string "text/event-stream, application/json")
             (url (format "%s://%s:%s%s"
                          (if (mcp--tls connection) "https" "http")
                          (mcp--host connection)
                          (mcp--port connection)
                          (if-let* ((endpoint (mcp--endpoint connection)))
                              endpoint
                            (mcp--path connection)))))
         (with-current-buffer
             (url-retrieve url
                           (lambda (_)
                             (when (buffer-live-p (current-buffer))
                               (goto-char (point-min))
                               (when (search-forward "\n\n" nil t)
                                 (let* ((headers (buffer-substring (point-min) (point)))
                                        (body (buffer-substring (point) (point-max)))
                                        (headers-plist (mcp--parse-http-header headers))
                                        (session-id (plist-get headers-plist :mcp-session-id))
                                        (response-code (plist-get headers-plist :response-code)))
                                   (when (string= "4"
                                                  (substring response-code 0 1))
                                     (setf (mcp--sse connection) t))
                                   (when session-id
                                     (setf (mcp--session-id connection)
                                           session-id))
                                   ;; connect sse
                                   (when (and (mcp--sse connection)
                                              (not (jsonrpc--process connection)))
                                     (mcp--connect-sse connection))
                                   (unless (mcp--sse connection)
                                     (when-let* ((content-type (plist-get headers-plist :content-type))
                                                 ((not (string= "" (string-trim body)))))
                                       (let (data json)
                                         (pcase (car (split-string content-type  ";" t))
                                           ("text/event-stream"
                                            (dolist (line (split-string body "\n"))
                                              (cond
                                               ((string-prefix-p "data: " line)
                                                (setq data (string-trim (substring line 5)))))))
                                           ("application/json"
                                            (setq data body)))
                                         (condition-case-unless-debug err
                                             (when (stringp data)
                                               (setq json (json-parse-string data
                                                                             :object-type 'plist
                                                                             :null-object nil
                                                                             :false-object :json-false)))
                                           (json-parse-error
                                            ;; parse error and not because of incomplete json
                                            (jsonrpc--warn "Invalid JSON: %s\t %s" (cdr err) data)))
                                         (when json
                                           (jsonrpc-connection-receive connection json)))))))
                               (kill-buffer))))
           (set (make-local-variable 'url-mime-accept-string) url-mime-accept-string))))
      ('stdio
       (process-send-string
        (jsonrpc--process connection)
        (format "%s\r\n" json))))
    (jsonrpc--event
     connection
     'client
     :json json
     :kind  kind
     :message args
     :foreign-message converted)))

(defvar mcp--in-process-filter nil
  "Non-nil if inside `mcp--process-filter'.")

(cl-defun mcp--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when mcp--in-process-filter
    ;; Problematic recursive process filters may happen if
    ;; `jsonrpc-connection-receive', called by us, eventually calls
    ;; client code which calls `process-send-string' (which see) to,
    ;; say send a follow-up message.  If that happens to writes enough
    ;; bytes for pending output to be received, we will lose JSONRPC
    ;; messages.  In that case, remove recursiveness by re-scheduling
    ;; ourselves to run from within a timer as soon as possible
    ;; (bug#60088)
    (run-at-time 0 nil #'mcp--process-filter proc string)
    (cl-return-from mcp--process-filter))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((conn (process-get proc 'jsonrpc-connection))
             (type (mcp--connection-type conn))
             (queue (or (process-get proc 'jsonrpc-mqueue) nil))
             (buf (or (process-get proc 'jsonrpc-pending)
                      (plist-get (process-put
                                  proc 'jsonrpc-pending
                                  (generate-new-buffer
                                   (format " *mcp-%s-jsonrpc-pending*" (jsonrpc-name conn))
                                   'inhibit-hooks))
                                 'jsonrpc-pending)))
             (message-rest-size (or (process-get proc 'jsonrpc-message-rest-size)
                                    0))
             (data (if (equal type 'stdio)
                       (with-current-buffer buf
                         (goto-char (point-max))
                         (insert string)
                         (buffer-string))
                     string))
             (parsed-messages nil)
             (separator (if (equal type 'stdio)
                            "\n"
                          (if (mcp--sse conn)
                              "\r\n\r\n"
                            "\n\n")))
             (data-blocks (split-string data separator)))
        (dolist (data-block data-blocks)
          (let ((data-block (string-trim data-block)))
            (unless (string-empty-p data-block)
              (pcase type
                ('http
                 (if (string-prefix-p "HTTP" data-block)
                     (if-let* ((headers (mcp--parse-http-header data-block))
                               (response-code (plist-get headers :response-code))
                               (content-type (plist-get headers :content-type))
                               (content-type (or (car (split-string content-type ";" t)) "")))
                         (when (or (not (string= response-code "200"))
                                   (not (string-match "text/event-stream" content-type)))
                           ;; sse not connect success
                           (message "sse not connect, return code: %s" response-code))
                       ;; can't parse headers
                       (message "can't parse headers: %s" data-block))
                   (if (= 0 message-rest-size)
                       (let* ((data-line (split-string data-block "\n"))
                              (data-size-line (cl-first data-line))
                              (event-line (cl-second data-line))
                              (id-line (when-let* ((id-line (cl-third data-line)))
                                         (if (string-prefix-p "id" id-line)
                                             id-line)))
                              (data-body (if id-line
                                             (cl-fourth data-line)
                                           (cl-third data-line)))
                              (data (when data-body
                                      (string-trim (substring data-body 6)))))
                         (when-let* ((event-line event-line)
                                     (data-size (string-to-number (string-trim data-size-line)
                                                                  16))
                                     (event-type (if (string-prefix-p ": ping" event-line)
                                                     'ping
                                                   (intern (string-trim (substring event-line 6)))))
                                     (body-size (let ((len 0))
                                                  (dolist (i (cdr data-line)) (setq len (+ len (length i))))
                                                  (+ len (length (cdr data-line)) -1)))
                                     (rest-size (- data-size
                                                   2 ; \r\n after data-size
                                                   ;; only sse need add 2
                                                   (if (mcp--sse conn)
                                                       2 ; \r\n after the last data-line
                                                     0)
                                                   body-size)))
                           (pcase event-type
                             ('endpoint
                              (let* ((endpoint (if (string-match "http://[^/]+\\(/[^[:space:]]+\\)" data)
                                                   (match-string 1 data)
                                                 data)))
                                (unless (mcp--endpoint conn)
                                  (setf (mcp--endpoint conn) endpoint)
                                  (mcp--send-initial-message conn))))
                             ('message
                              (if (>= 0 rest-size)
                                  (push data
                                        parsed-messages)
                                (process-put proc 'jsonrpc-message-rest-size rest-size)
                                (with-current-buffer buf
                                  (goto-char (point-max))
                                  (insert data))))
                             (_))))
                     (let* ((data-block-size (length data-block))
                            (new-message-rest-size (- message-rest-size data-block-size)))
                       (process-put proc 'jsonrpc-message-rest-size new-message-rest-size)
                       (with-current-buffer buf
                         (goto-char (point-max))
                         (insert (string-trim data-block))
                         (when (= 0 new-message-rest-size)
                           (push (buffer-string)
                                 parsed-messages)
                           (erase-buffer)))))))
                ('stdio
                 (push data-block parsed-messages))))))

        (setq parsed-messages (nreverse parsed-messages))

        (when (equal type 'stdio)
          (with-current-buffer buf (erase-buffer)))
        ;; Add messages to MQUEUE
        (dolist (msg parsed-messages)
          (let ((json nil)
                (json-str (with-current-buffer buf
                            (if (= (point-min) (point-max))
                                msg
                              (goto-char (point-max))
                              (insert msg)
                              (buffer-string)))))
            (condition-case-unless-debug err
                (when (stringp json-str)
                  (setq json
                        (json-parse-string (decode-coding-string json-str 'utf-8)
                                           :object-type 'plist
                                           :null-object nil
                                           :false-object :json-false)))
              (json-parse-error
               ;; parse error and not because of incomplete json
               (jsonrpc--warn "Invalid JSON: %s\t %s" (cdr err) json-str))
              (json-end-of-file
               ;; Save remaining data to pending for next processing
               (with-current-buffer buf
                 (goto-char (point-max))
                 (insert json-str)
                 (process-put proc 'jsonrpc-pending buf))))
            (when json
              (when (equal type 'stdio)
                (with-current-buffer buf (erase-buffer)))
              (when (listp json)
                (setq json (plist-put json :jsonrpc-json json-str))
                (push json queue)))))

        ;; Save updated queue
        (process-put proc 'jsonrpc-mqueue queue)

        ;; Dispatch messages in timer
        (cl-loop with time = (current-time)
                 for msg = (pop queue) while msg
                 do (let ((timer (timer-create)))
                      (timer-set-time timer time)
                      (timer-set-function timer
                                          (lambda (conn msg)
                                            (with-temp-buffer
                                              (jsonrpc-connection-receive conn msg)))
                                          (list conn msg))
                      (timer-activate timer)))

        ;; Save final queue (might have been consumed by timer pop)
        (process-put proc 'jsonrpc-mqueue queue)))))

(cl-defmethod mcp--connect-sse ((conn mcp-http-process-connection))
  "Establish SSE (Server-Sent Events) connection for HTTP CONN."
  (let* ((name (jsonrpc-name conn))
         (buffer-name (format "*Mcp %s server*" name))
         (process-name (format "mcp-%s-server" name))
         (host (mcp--host conn))
         (port (mcp--port conn))
         (path (mcp--path conn))
         (proc (progn
                 (get-buffer-create buffer-name)
                 (open-network-stream process-name
                                      buffer-name
                                      host
                                      port
                                      :type (if (mcp--tls conn)
                                                'tls
                                              'network)
                                      :coding 'utf-8-unix))))
    (let* ((stderr-buffer-name (format "*%s stderr*" name))
           (stderr-buffer (jsonrpc--forwarding-buffer stderr-buffer-name "[stderr] " conn))
           (hidden-name (concat " " stderr-buffer-name)))
      (with-current-buffer stderr-buffer
        (ignore-errors (kill-buffer hidden-name))
        (rename-buffer hidden-name)
        (setq buffer-read-only t))
      (process-put proc 'jsonrpc-stderr stderr-buffer))
    (setf (jsonrpc--process conn) proc)
    (set-process-buffer proc (get-buffer-create (format " *%s output*" name)))
    (set-process-filter proc #'mcp--process-filter)
    (set-process-sentinel proc #'jsonrpc--process-sentinel)
    (with-current-buffer (process-buffer proc)
      (buffer-disable-undo)
      (set-marker (process-mark proc) (point-min))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only t))
    (process-put proc 'jsonrpc-connection conn)

    (process-send-string proc
                         (concat
                          (format "GET %s HTTP/1.1\r\n"
                                  path)
                          (format "Host: %s:%s\r\n"
                                  host
                                  port)
                          "Accept: text/event-stream\r\n"
                          (when-let* ((token (mcp--resolve-value (mcp--token conn))))
                            (format "Authorization: Bearer %s\r\n" token))
                          (unless (mcp--sse conn)
                            (format "Mcp-Session-Id: %s\r\n" (mcp--session-id conn)))
                          "Cache-Control: no-cache\r\n"
                          "Connection: keep-alive\r\n\r\n"))))


;; The handshake lives in mcp.el: this file only opens transports and
;; delivers already-framed messages to the JSON-RPC layer.
(declare-function mcp--send-initial-message "mcp" (connection &optional check-sse syncp))

(provide 'mcp-transport)
;;; mcp-transport.el ends here
