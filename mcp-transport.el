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
;; JSON-RPC send method for stdio and Streamable HTTP, the stdio framing
;; filter, the streamed HTTP response reader, and the request headers the
;; stateless revision requires.

;;; Code:

(require 'jsonrpc)
(require 'cl-lib)
(require 'url)
(require 'plz)

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
   (-instructions
    :initform nil
    :accessor mcp--instructions)
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
   (-list-cache
    :initform nil
    :documentation "Cache of list responses, keyed by JSON-RPC method.
Each value holds the items and, when the server supplied a positive
ttlMs, the time at which they stop being fresh.  The table starts as
nil and `mcp--list-cache' creates it, because an `:initform' holding a
hash table is evaluated once for the whole class and would then be
shared by every connection.")
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
   (-running
    :initform t
    :accessor mcp--running))
  :documentation "An MCP connection over Streamable HTTP.")

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

(defun mcp--list-cache (connection)
  "Return the list cache of CONNECTION, creating it on first use.
The table belongs to one connection.  A shared one would let the list
answer of one server answer a call that another server never saw."
  (or (slot-value connection '-list-cache)
      (setf (slot-value connection '-list-cache)
            (make-hash-table :test #'equal))))

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

;;; Per-request protocol metadata

;; The 2026-07-28 revision is stateless: every request repeats the protocol
;; version, the client capabilities and the client identity.  Nothing here may
;; depend on a previous request, so the values travel with each message.

(defconst mcp--protocol-version "2026-07-28"
  "MCP protocol revision this client sends with every request.")

(defconst mcp--client-info '(:name "mcp-emacs" :version "0.1.0")
  "Client identity reported in `io.modelcontextprotocol/clientInfo'.")

(defconst mcp--meta-reserved-keys
  '(:io.modelcontextprotocol/protocolVersion
    :io.modelcontextprotocol/clientCapabilities
    :io.modelcontextprotocol/clientInfo)
  "Metadata keys the client sets itself.
Caller-supplied duplicates are dropped so a request never carries two
values for the same reserved key.")

(defun mcp--hash-table-to-plist (table)
  "Return hash TABLE as a plist.
Keywords stay keywords and everything else becomes a keyword, which is
what `json-serialize' writes as an object key."
  (let (plist)
    (maphash (lambda (key value)
               (let ((key (cond ((keywordp key) key)
                                ((symbolp key) (intern (concat ":" (symbol-name key))))
                                (t (intern (concat ":" (format "%s" key)))))))
                 (push key plist)
                 (push value plist)))
             table)
    (nreverse plist)))

(defun mcp--params-to-plist (params)
  "Return PARAMS as a plist.
PARAMS comes from callers in three shapes: nil, a plist, or a hash
table (the list requests pass an empty one).  Merging metadata needs a
single representation."
  (cond ((null params) nil)
        ((hash-table-p params) (mcp--hash-table-to-plist params))
        ((listp params) params)
        (t (error "Unsupported JSON-RPC params: %S" params))))

(defun mcp--request-meta (&optional caller-meta)
  "Return the `_meta' plist for a request.
CALLER-META holds extra keys the caller wants to attach, such as a
`progressToken'.  Its unknown keys survive.  Reserved keys come from
this client only."
  (let ((caller (mcp--params-to-plist caller-meta))
        ;; Capabilities are objects, so empty ones serialize as {} rather
        ;; than null.  The client answers `roots/list' during a multi
        ;; round-trip request, which is the only capability it declares.
        (capabilities (make-hash-table :test #'equal)))
    (puthash "roots" (make-hash-table :test #'equal) capabilities)
    (append
     (cl-loop for (key value) on caller by #'cddr
              unless (memq key mcp--meta-reserved-keys)
              append (list key value))
     (list :io.modelcontextprotocol/protocolVersion mcp--protocol-version
           :io.modelcontextprotocol/clientCapabilities capabilities
           :io.modelcontextprotocol/clientInfo (copy-sequence mcp--client-info)))))

(defun mcp--request-params (params)
  "Return PARAMS as a plist with per-request protocol metadata added."
  (let* ((params (mcp--params-to-plist params))
         (meta (plist-get params :_meta))
         (body (cl-loop for (key value) on params by #'cddr
                        unless (eq key :_meta)
                        append (list key value))))
    (append body (list :_meta (mcp--request-meta meta)))))

;;; HTTP responses

(defun mcp--parse-json-message (string)
  "Return the JSON object in STRING, or nil when STRING does not parse."
  (condition-case-unless-debug err
      (json-parse-string string
                         :object-type 'plist
                         :null-object nil
                         :false-object :json-false)
    (json-parse-error
     (jsonrpc--warn "Invalid JSON: %s\t %s" (cdr err) string)
     nil)
    (json-end-of-file nil)))

(defun mcp--sse-payloads (body)
  "Return the data payload of each event in the server-sent event BODY."
  (let (payloads
        (data nil))
    (dolist (line (split-string body "\r\n\\|\n"))
      (cond
       ((string-empty-p (string-trim line))
        (when data
          (push (string-join (nreverse data) "\n") payloads)
          (setq data nil)))
       ((string-prefix-p "data:" line)
        ;; A single space after the colon is part of the syntax, not the value.
        (push (string-remove-prefix " " (substring line 5)) data))))
    (when data
      (push (string-join (nreverse data) "\n") payloads))
    (nreverse payloads)))

(defun mcp--response-payloads (content-type body)
  "Return the JSON payloads carried by BODY of CONTENT-TYPE.
An empty list means the response carries no message.  A server that omits
or mislabels the content type still means to send a JSON-RPC message, so
a body that starts like a JSON object counts as JSON."
  (pcase (car (split-string (or content-type "") ";" t))
    ("application/json" (list body))
    ("text/event-stream" (mcp--sse-payloads body))
    (_ (when (string-prefix-p "{" (string-trim-left (or body "")))
         (list body)))))

(defun mcp--http-endpoint (connection)
  "Return the endpoint URL of CONNECTION."
  (format "%s://%s:%s%s"
          (if (mcp--tls connection) "https" "http")
          (mcp--host connection)
          (mcp--port connection)
          (mcp--path connection)))

(defun mcp--http-deliver (connection request-id payload)
  "Hand PAYLOAD to CONNECTION.
Return non-nil when PAYLOAD answers the request REQUEST-ID."
  (when (and (stringp payload)
             (not (string-empty-p (string-trim payload))))
    (when-let* ((message (mcp--parse-json-message payload)))
      (jsonrpc-connection-receive connection message)
      (and request-id (equal (plist-get message :id) request-id)))))

(defvar mcp--http-streams (make-hash-table :test #'eq)
  "Response streams in flight.
The outer table maps a connection to its inner table, which maps a
JSON-RPC request id to the state of that response.  Keying by connection
matters: every connection numbers its own requests from one, so a table
keyed by id alone would let two servers overwrite each other's streams.
Each state is a plist with :connection, :pending, :phase, :headers,
:status, :content-type, :delivered and :process.")

(defun mcp--http-streams-of (connection)
  "Return the request table of CONNECTION, creating it when absent."
  (or (gethash connection mcp--http-streams)
      (puthash connection (make-hash-table :test #'equal)
               mcp--http-streams)))

(defun mcp--http-stream-get (connection request-id)
  "Return the stream state of REQUEST-ID on CONNECTION, or nil."
  (and request-id
       (gethash request-id (mcp--http-streams-of connection))))

(defun mcp--http-stream-put (connection request-id state)
  "Remember STATE for REQUEST-ID on CONNECTION."
  (puthash request-id state (mcp--http-streams-of connection)))

(defconst mcp--http-stream-table-limit 256
  "Stream table size that triggers a sweep of finished entries.")

(defun mcp--http-prune-streams (connection)
  "Drop the finished streams of CONNECTION once its table grows."
  (let ((streams (mcp--http-streams-of connection)))
    (when (> (hash-table-count streams)
             mcp--http-stream-table-limit)
      (let (finished)
        (maphash (lambda (key value)
                   (when (eq value :finished)
                     (push key finished)))
                 streams)
        (dolist (key finished)
          (remhash key streams))))))

(defun mcp--http-consume (connection request-id proc text)
  "Consume TEXT from the response stream running on PROC.
Return the updated stream state.  The state starts in a header phase,
because curl writes the status line and headers first, and moves to a
body phase once a non-redirect header block arrives.  A stream that
already finished ignores late output."
  (let ((state (mcp--http-stream-get connection request-id)))
    (if (eq state :finished)
        state
      (let* ((state (or state (list :phase 'headers :pending ""
                                    :connection connection)))
             (pending (concat (or (plist-get state :pending) "") text)))
        (setq state (plist-put state :pending pending))
        ;; Header blocks: 1xx and 3xx responses carry no message of their own.
        (while (and (eq (plist-get state :phase) 'headers)
                    (string-match "\r?\n\r?\n" (plist-get state :pending)))
          (let* ((pending (plist-get state :pending))
                 (raw (substring pending 0 (match-beginning 0)))
                 (rest (substring pending (match-end 0)))
                 (headers (mcp--parse-http-header raw))
                 (status (plist-get headers :response-code)))
            (setq state (plist-put state :pending rest))
            (if (and (stringp status) (string-match-p "\\`[13]" status))
                (setq state (plist-put state :headers nil))
              (setq state (plist-put state :headers headers))
              (setq state (plist-put state :status status))
              (setq state (plist-put state :content-type
                                     (plist-get headers :content-type)))
              (setq state (plist-put state :phase 'body)))))
        ;; Body: server-sent events arrive incrementally, JSON arrives whole.
        (when (eq (plist-get state :phase) 'body)
          (let ((kind (car (split-string (or (plist-get state :content-type) "")
                                         ";" t))))
            (when (equal kind "text/event-stream")
              (let* ((pending (plist-get state :pending))
                     (parts (split-string pending "\r?\n\r?\n"))
                     (rest (car (last parts))))
                (dolist (block (butlast parts))
                  (dolist (payload (mcp--sse-payloads block))
                    (when (mcp--http-deliver connection request-id payload)
                      (setq state (plist-put state :delivered t)))))
                (setq state (plist-put state :pending rest))))))
        (setq state (plist-put state :process proc))
        (when request-id
          (mcp--http-stream-put connection request-id state))
        state))))

(defun mcp--http-report-failure (connection request-id message)
  "Report MESSAGE as the failure of REQUEST-ID on CONNECTION.
Without this the request would sit until the JSON-RPC timeout expired,
because a transport failure produces no response of its own."
  (when request-id
    (jsonrpc-connection-receive
     connection
     (list :jsonrpc "2.0"
           :id request-id
           :error (list :code -32000 :message message)))))

(defun mcp--http-finish (connection request-id)
  "Flush and close the response stream of REQUEST-ID.
A nil REQUEST-ID marks a notification, which expects no response."
  (let ((state (mcp--http-stream-get connection request-id)))
    (cond
     ;; plz reports success, failure and completion separately; only the
     ;; first of them may act.
     ((eq state :finished) nil)
     ((null state)
      (mcp--http-report-failure connection request-id
                                (format "no response from %s"
                                        (mcp--http-endpoint connection))))
     (t
      (mcp--http-stream-put connection request-id :finished)
      (let ((pending (plist-get state :pending)))
        (when (and (stringp pending)
                   (not (string-empty-p (string-trim pending))))
          (dolist (payload (mcp--response-payloads
                            (plist-get state :content-type)
                            pending))
            (when (mcp--http-deliver connection request-id payload)
              (setq state (plist-put state :delivered t)))))
        (unless (or (plist-get state :delivered) (not request-id))
          (mcp--http-report-failure
           connection request-id
           (format "HTTP %s from %s"
                   (or (plist-get state :status) "error")
                   (mcp--http-endpoint connection))))
        ;; The curl process and its buffer belong to plz, which closes
        ;; both once it reports the response.
        nil)))))

;;; HTTP request headers

;; The transport mirrors parts of the JSON-RPC body into headers so that
;; intermediaries can route and observe requests without parsing the body.
;; Values that cannot appear verbatim in a header travel as base64 between
;; the markers below.

(defconst mcp--header-sentinel-prefix "=?base64?"
  "Marker that starts a base64-encoded header value.")

(defconst mcp--header-sentinel-suffix "?="
  "Marker that ends a base64-encoded header value.")

(defconst mcp--methods-with-name '("tools/call" "prompts/get" "resources/read")
  "Requests whose body carries a name or URI that mirrors into Mcp-Name.")

(defun mcp--header-value (value)
  "Return VALUE encoded for an MCP request header.
Only visible ASCII, space and tab may appear verbatim.  Anything else,
any value holding a quote or a backslash, and any value that looks like
the marker syntax itself travels as base64 of its UTF-8 bytes: plz
writes headers into a curl config file, where a quote or a backslash
would end the argument."
  (let ((string (format "%s" value)))
    (if (or (string-match-p "[^\t !-~]" string)
            (string-search "\"" string)
            (string-search "\\" string)
            (and (string-prefix-p mcp--header-sentinel-prefix string)
                 (string-suffix-p mcp--header-sentinel-suffix string)))
        (format "%s%s%s"
                mcp--header-sentinel-prefix
                (base64-encode-string (encode-coding-string string 'utf-8) t)
                mcp--header-sentinel-suffix)
      string)))

;;; Tool parameters mirrored into headers

;; A server may mark tool parameters with x-mcp-header, and the client must
;; then send those argument values as Mcp-Param-{name} headers.  A tool whose
;; annotations break the rules must not appear in tools/list at all.

(define-error 'mcp-invalid-tool "MCP tool definition rejected")

(defun mcp--valid-header-name-p (name)
  "Return non-nil when NAME is an HTTP field-name token.
Token characters come from RFC 9110 section 5.1."
  (and (stringp name)
       (not (string-empty-p name))
       (let ((start (string-match "[!#$%&'*+.^_`|~0-9A-Za-z-]+" name)))
         (and (eq start 0)
              (= (match-end 0) (length name))))))

(defconst mcp--header-integer-limit 9007199254740991
  "Largest integer JavaScript represents exactly.")

(defun mcp--schema-annotation-count (node)
  "Count every x-mcp-header key in NODE at any depth."
  (cond
   ((vectorp node)
    (cl-loop for item across node
             sum (mcp--schema-annotation-count item)))
   ((and (consp node) (keywordp (car node)))
    (cl-loop for (key value) on node by #'cddr
             sum (+ (if (eq key :x-mcp-header) 1 0)
                    (mcp--schema-annotation-count value))))
   (t 0)))

(defun mcp--property-annotations (schema path)
  "Collect (PATH . NAME) for x-mcp-header annotations in SCHEMA.
PATH is the property chain from the schema root.  Only chains of
`properties' keys are followed, which is what the specification calls
statically reachable."
  (let (annotations
        (properties (plist-get schema :properties)))
    (cl-loop for (key value) on properties by #'cddr
             do (let ((header (plist-get value :x-mcp-header))
                      (sub-path (append path (list key))))
                  (when header
                    (push (cons sub-path header) annotations))
                  (when (plist-get value :properties)
                    (setq annotations
                          (append (mcp--property-annotations value sub-path)
                                  annotations)))))
    annotations))

(defun mcp--schema-at-path (schema path)
  "Return the subschema of SCHEMA at PATH."
  (let ((node schema))
    (dolist (key path node)
      (setq node (plist-get (plist-get node :properties) key)))))

(defun mcp--invalid-tool (format-string &rest args)
  "Signal `mcp-invalid-tool' with FORMAT-STRING and ARGS."
  (signal 'mcp-invalid-tool (list (apply #'format format-string args))))

(defun mcp--check-header-name (name tool-name)
  "Signal unless NAME is a usable header name for TOOL-NAME."
  (unless (mcp--valid-header-name-p name)
    (mcp--invalid-tool "tool %s annotates a parameter with an invalid header name %S"
                       tool-name name)))

(defun mcp--check-header-type (schema name tool-name)
  "Signal unless the type of SCHEMA suits a header called NAME.
TOOL-NAME names the tool under inspection."
  (let ((type (plist-get schema :type)))
    (unless (member type '("string" "boolean" "integer"))
      (mcp--invalid-tool
       "tool %s mirrors %s into a header but its type %S cannot be sent"
       tool-name name (or type "missing")))))

(defun mcp--tool-header-fields (tool)
  "Return (PATH . HEADER-NAME) pairs for the x-mcp-header of TOOL.
Signal `mcp-invalid-tool' when an annotation breaks a rule, which makes
the whole tool definition invalid."
  (let* ((tool-name (plist-get tool :name))
         (schema (plist-get tool :inputSchema))
         (annotations (mcp--property-annotations schema nil))
         (names nil))
    (unless (= (length annotations)
               (mcp--schema-annotation-count schema))
      (mcp--invalid-tool
       "tool %s annotates a parameter that is not statically reachable"
       tool-name))
    (dolist (annotation annotations)
      (let ((name (cdr annotation)))
        (mcp--check-header-name name tool-name)
        (mcp--check-header-type (mcp--schema-at-path schema (car annotation))
                                name tool-name)
        (push name names)))
    (unless (= (length names)
               (length (delete-dups (mapcar #'downcase names))))
      (mcp--invalid-tool
       "tool %s uses one header name for different parameters"
       tool-name))
    annotations))

(defun mcp--validated-tools (tools)
  "Return TOOLS without definitions the transport must reject.
TOOLS is a vector of tool definitions."
  (let ((kept []))
    (dolist (tool (append tools nil))
      (condition-case err
          (progn
            (mcp--tool-header-fields tool)
            (setq kept (vconcat kept (vector tool))))
        (error
         (message "[mcp] rejecting tool %s: %s"
                  (plist-get tool :name)
                  (error-message-string err)))))
    kept))

(defun mcp--path-value (arguments path)
  "Return the value of PATH inside ARGUMENTS, or nil when absent."
  (let ((node arguments))
    (dolist (key path node)
      (setq node (and (consp node) (plist-get node key))))))

(defun mcp--header-mirrorable-p (value name tool-name)
  "Return non-nil when VALUE may travel in the header called NAME.
Only values JavaScript represents exactly may be mirrored, so an integer
beyond that range is dropped with a warning instead of being sent."
  (cond
   ((null value) nil)
   ((integerp value)
    (if (<= (abs value) mcp--header-integer-limit)
        t
      (message "[mcp] not mirroring %s of %s into %s: outside the safe integer range"
               value tool-name name)
      nil))
   (t t)))

(defun mcp--header-scalar (value)
  "Return VALUE as the text the JSON body shows for it, or nil.
A JSON true arrives as `t' and a JSON false as `:json-false', so a
header must say \"true\" or \"false\", never \"t\" or \":json-false\".
Nil stands for an argument the caller left out, which produces no
header."
  (cond ((eq value t) "true")
        ((eq value :json-false) "false")
        ((null value) nil)
        ((numberp value) (number-to-string value))
        (t (format "%s" value))))

(defun mcp--tool-param-headers (connection tool-name arguments)
  "Return Mcp-Param headers for TOOL-NAME called with ARGUMENTS.
TOOL-NAME is looked up in the tools of CONNECTION, which carry the
annotations that name each header.  A tool whose annotations stopped
being valid yields no headers instead of breaking the send."
  (when-let* ((tool (cl-find tool-name
                            (mcp--tools connection)
                            :test #'equal
                            :key (lambda (tool) (plist-get tool :name)))))
    (condition-case err
        (cl-loop for (path . name) in (mcp--tool-header-fields tool)
                 for value = (mcp--path-value arguments path)
                 for text = (mcp--header-scalar value)
                 when (and text
                           (mcp--header-mirrorable-p value name tool-name))
                 collect (cons (concat "Mcp-Param-" name)
                               (mcp--header-value text)))
      (mcp-invalid-tool
       (message "[mcp] not mirroring parameters of %s: %s"
                tool-name (error-message-string err))
       nil))))

(defun mcp--http-headers (connection args)
  "Return the request headers for ARGS on CONNECTION.
ARGS holds the JSON-RPC method, parameters and id of the message."
  (let* ((method (plist-get args :method))
         (params (plist-get args :params))
         (name (when (member method mcp--methods-with-name)
                 (or (plist-get params :name)
                     (plist-get params :uri)))))
    (append
     '(("Content-Type" . "application/json")
       ("Accept" . "application/json, text/event-stream"))
     (list (cons "MCP-Protocol-Version" mcp--protocol-version))
     ;; A reply carries no method, so it gets no method header.
     (when method
       (list (cons "Mcp-Method" method)))
     (when name
       (list (cons "Mcp-Name" (mcp--header-value name))))
     (when-let* ((token (mcp--resolve-value (mcp--token connection))))
       (list (cons "Authorization" (concat "Bearer " token))))
     (mcp--headers connection)
     (when (equal method "tools/call")
       (mcp--tool-param-headers connection
                                (plist-get params :name)
                                (plist-get params :arguments))))))

(defconst mcp--cancelled-code -32800
  "Error code reported locally for a request the client cancelled.")

(defun mcp--http-cancel (connection request-id)
  "Close the response stream of REQUEST-ID on CONNECTION.
Return non-nil when a stream was open.  Closing the stream is how the
transport withdraws a request: the revision has no resumable streams and
no session to cancel within.  plz leaves a deleted process alone, so this
closes its buffer too."
  (let ((state (mcp--http-stream-get connection request-id)))
    (when (and state (not (eq state :finished)))
      (when-let* ((proc (plist-get state :process)))
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p (process-buffer proc))
          (kill-buffer (process-buffer proc))))
      (mcp--http-stream-put connection request-id :finished)
      t)))

(defun mcp--http-cancel-streams (connection)
  "Close every response stream belonging to CONNECTION."
  (let (ids)
    (maphash (lambda (request-id state)
               (unless (eq state :finished)
                 (push request-id ids)))
             (mcp--http-streams-of connection))
    (dolist (request-id ids)
      (mcp--http-cancel connection request-id))))

(defun mcp--http-post (connection body headers request-id)
  "POST BODY to CONNECTION with HEADERS.
REQUEST-ID identifies the request the response carries, or nil for a
notification.  Responses stream back into the JSON-RPC layer as they
arrive, so a server may send progress notifications before the reply."
  (when request-id
    (mcp--http-prune-streams connection)
    (mcp--http-stream-put connection request-id
                          (list :connection connection :phase 'headers
                                :pending "")))
  (let ((plz-curl-default-args (append plz-curl-default-args '("--no-buffer"))))
    (plz 'post
         (mcp--http-endpoint connection)
         :headers headers
         :body body
         :body-type 'text
         :as 'response
         :timeout (mcp--timeout connection)
         :connect-timeout 10
         :filter (lambda (proc text)
                   ;; plz parses the response from the process buffer, so
                   ;; the text must land there as well as in this reader.
                   (when (buffer-live-p (process-buffer proc))
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-max))
                       (insert text)))
                   (mcp--http-consume connection request-id proc text))
         ;; Whether curl succeeded or not, the stream ends here.  A JSON-RPC
         ;; error body already reached the connection through the filter, so
         ;; a non-2xx status needs no separate report.
         :else (lambda (_err) (mcp--http-finish connection request-id))
         :then (lambda (_response) (mcp--http-finish connection request-id))
         :finally (lambda () (mcp--http-finish connection request-id)))))

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
- HTTP connections send one POST per message
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
         ;; Requests carry the protocol version, capabilities and client
         ;; identity.  Replies and notifications stay untouched: the protocol
         ;; defines the metadata for requests only.
         (args (if (eq kind 'request)
                   (plist-put args :params (mcp--request-params (plist-get args :params)))
                 args))
         (converted (jsonrpc-convert-to-endpoint connection args kind))
         (json (json-serialize converted
                               :false-object :json-false
                               :null-object :json-null)))
    (pcase (mcp--connection-type connection)
      ('http
       (mcp--http-post connection
                       json
                       (mcp--http-headers connection args)
                       ;; Only a request owns a response stream.  A reply
                       ;; carries an id too, but nothing will answer it.
                       (when (eq kind 'request) id)))
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
  "Non-nil while inside `mcp--process-filter'.")

(cl-defun mcp--process-filter (proc string)
  "Collect the newline-delimited JSON messages in STRING from PROC.
Dispatch happens outside the filter, because client code handling a
message may send another one."
  (when mcp--in-process-filter
    ;; A recursive filter can happen when dispatching a message makes client
    ;; code send another message.  Defer instead of losing data (bug#60088).
    (run-at-time 0 nil #'mcp--process-filter proc string)
    (cl-return-from mcp--process-filter))
  (let ((mcp--in-process-filter t))
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let* ((conn (process-get proc 'jsonrpc-connection))
               (queue (or (process-get proc 'jsonrpc-mqueue) nil))
               (pending (or (process-get proc 'jsonrpc-pending) ""))
               (lines (split-string (concat pending string) "\n"))
               ;; Text after the last newline is an incomplete message.
               (rest (car (last lines))))
          (dolist (line (butlast lines))
            (let ((line (string-trim line)))
              (unless (string-empty-p line)
                (when-let* ((message (mcp--parse-json-message line)))
                  (push message queue)))))
          (process-put proc 'jsonrpc-pending rest)
          (process-put proc 'jsonrpc-mqueue queue)

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

          (process-put proc 'jsonrpc-mqueue queue))))))

(provide 'mcp-transport)
;;; mcp-transport.el ends here
