;;; mcp.el --- Model Context Protocol                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (jsonrpc "1.0.25"))
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

;; mcp is an Emacs client for interfacing with
;; [MCP](https://modelcontextprotocol.io/introduction), supporting
;; connections to MCP servers.
;;
;; Features:
;;
;; - Structured communication with MCP servers
;; - Support for filesystem and generic MCP servers
;; - Extensible tool and prompt system
;; - Asynchronous and synchronous operations
;; - Resource management capabilities
;; - Intuitive interface for managing server lifecycle (start/stop/restart)
;; - Integration with popular Emacs packages (e.g., gptel, llm)
;;
;; Usage:
;; - config `mcp-hub-servers'
;; - call `mcp-hub' to start mcp servers

;;; Code:

(require 'jsonrpc)
(require 'cl-lib)
(require 'url)
(require 'mcp-transport)

(defcustom mcp-server-start-time 60
  "The Seconds of mcp server start time."
  :group 'mcp
  :type 'integer)

(defcustom mcp-cache-lists t
  "Whether to reuse a list response while its ttlMs stays fresh.
Servers declare freshness in milliseconds on tools/list, prompts/list,
resources/list and resources/templates/list.  A nil value asks the server
again on every list call."
  :group 'mcp
  :type 'boolean)

(defcustom mcp-log-size nil
  "Maximum size for logging jsonrpc event.  0 disables, nil means infinite."
  :group 'mcp
  :type 'integer)


(cl-defun mcp-notify (connection method &optional (params nil))
  "Send notification to CONNECTION without expecting response.
METHOD is the notification name (string or symbol). PARAMS is an
optional plist of parameters.
This is a thin wrapper around =jsonrpc-connection-send' that
omits the :id parameter to indicate it's a notification rather
than a request."
  (apply #'jsonrpc-connection-send
         `(,connection
           :method ,method
           ,@(when params
               (list :params params)))))

(defvar mcp-server-connections (make-hash-table :test #'equal)
  "Mcp server process.")

(defun mcp--roots-result (roots)
  "Return ROOTS as a ListRootsResult plist.
ROOTS holds directory strings or ready-made root plists."
  (list :roots
        (if roots
            (vconcat (mapcar (lambda (root)
                               (if (stringp root)
                                   (list :uri (concat "file://" (expand-file-name root))
                                         :name (file-name-nondirectory
                                                (directory-file-name root)))
                                 root))
                             roots))
          [])))

(defun mcp--reject-server-request (name method _params)
  "Refuse an unsolicited request from server NAME.
The stateless revision replaced server-initiated requests with multi
round-trip requests, so a server sending one contradicts the protocol
version it answered with.  METHOD names the refused request."
  (jsonrpc-error :code -32601
                 :message (format "server %s sent %s, which this protocol revision does not allow"
                                  name method)))

(defun mcp-notification-dispatcher (connection name method params)
  "Handle notifications from MCP server.
CONNECTION is the JSON-RPC connection object. NAME identifies the
server. METHOD is the notification name. PARAMS contains the
notification data."
  (pcase method
    ((or 'notifications/tools/list_changed
         'notifications/prompts/list_changed
         'notifications/resources/list_changed)
     (when-let* ((connection (gethash name mcp-server-connections)))
       ;; The notification is an invalidation signal: drop the cached list
       ;; and reload, so a hub buffer does not keep showing stale entries.
       (pcase method
         ('notifications/tools/list_changed
          (remhash :tools/list (mcp--list-cache connection))
          (mcp-async-list-tools connection (mcp--tools-callback connection)))
         ('notifications/prompts/list_changed
          (remhash :prompts/list (mcp--list-cache connection))
          (mcp-async-list-prompts connection (mcp--prompts-callback connection)))
         ('notifications/resources/list_changed
          (remhash :resources/list (mcp--list-cache connection))
          (remhash :resources/templates/list (mcp--list-cache connection))
          (mcp-async-list-resources connection (mcp--resources-callback connection))
          (mcp-async-list-resource-templates connection
                                             (mcp--resources-templates-callback connection))))))
    ('notifications/message
     (cond ((or (plist-member (mcp--capabilities connection) :logging)
                (and (plist-member params :level)
                     (plist-member params :data)))
            (cl-destructuring-bind (&key level data &allow-other-keys) params
              (let ((logger (plist-get params :logger)))
                (message "[mcp][%s][%s]%s: %s"
                         name
                         level
                         (if logger
                             (format "[%s]" logger)
                           "")
                         data))))))
    (_
     (message "%s Received notification: method=%s, params=%s" name method params))))

(defun mcp-on-shutdown (name)
  "When NAME mcp server shutdown."
  (message "%s connection shutdown" name))

(defun mcp--parse-http-url (url)
  "Parse HTTP/HTTPS URL into connection components.
URL should be a string in format http(s)://host[:port][/path].

Returns a plist with connection parameters:
:tls   - Boolean indicating HTTPS (t) or HTTP (nil)
:host  - Server hostname (string)
:port  - Port number (integer, defaults to 80/443)
:path  - URL path component (string)

Returns nil if URL is invalid or not HTTP/HTTPS."
  (when-let* ((url (url-generic-parse-url url))
              (type (url-type url))
              (host (url-host url))
              (filename (url-filename url)))
    (when (or (string= type "http")
              (string= type "https"))
      (let ((port (url-port url))
            (tls (string= "https" type)))
        (list :tls tls
              :host host
              :port (if port
                        port
                      (if tls
                          443
                        80))
              :path filename)))))

(defun mcp--discover (connection syncp callback error-callback)
  "Ask CONNECTION which protocol versions and capabilities it supports.
CALLBACK receives the result plist.  ERROR-CALLBACK receives a code
and a message.

`server/discover' is an ordinary request, not a handshake.  A modern
server answers it from scratch, and the answer also gives us the
server identity that the removed `initialize' response used to carry."
  (let* ((request-args `(,connection
                         :server/discover
                         nil
                         :timeout ,mcp-server-start-time))
         (async-args (append request-args
                             (list :success-fn callback
                                   :error-fn
                                   (jsonrpc-lambda (&key code message data)
                                     (funcall error-callback code message data))
                                   :timeout-fn
                                   (lambda ()
                                     (funcall error-callback 124 "timeout"))))))
    (if syncp
        (condition-case-unless-debug err
            (progn
              (funcall callback (apply #'jsonrpc-request request-args))
              t)
          (jsonrpc-error
           (pcase err
             (`(jsonrpc-error ,_ (jsonrpc-error-code . ,code) (jsonrpc-error-message . ,message) (jsonrpc-error-data . ,data))
              (funcall error-callback code message data))
             (`(jsonrpc-error ,_ (jsonrpc-error-message . "Timed out"))
              (funcall error-callback 124 "timeout" nil)))
           nil))
      (apply #'jsonrpc-async-request async-args))))

(defconst mcp--unsupported-protocol-version -32022
  "JSON-RPC error code for a protocol version the server rejects.")

(defun mcp--supported-versions-of (data)
  "Return the supported protocol versions named in error DATA.
DATA is the optional `data' member of a JSON-RPC error object."
  (append (plist-get data :supported) nil))

(defun mcp--connect-failed (connection code message &optional data)
  "Report CODE and MESSAGE for CONNECTION and stop it.
DATA is the optional error object payload.  For an unsupported protocol
version it names the versions the server accepts, which is the only way
the user learns what to aim for."
  (mcp-stop-server (jsonrpc-name connection))
  (setf (mcp--status connection) 'error)
  (let* ((supported (mcp--supported-versions-of data))
         (message (if (and (= code mcp--unsupported-protocol-version)
                           supported)
                      (format "%s (server supports: %s)"
                              message
                              (string-join supported ", "))
                    message)))
    (when (mcp--error-callback connection)
      (funcall (mcp--error-callback connection) code message))
    (message "Sadly, %s mpc server reports %s: %s"
             (jsonrpc-name connection) code message)))

(defun mcp--on-discovered (connection result syncp)
  "Apply the `server/discover' RESULT to CONNECTION and load its lists."
  (let* ((versions (append (plist-get result :supportedVersions) nil))
         (capabilities (plist-get result :capabilities))
         (server-info (plist-get (plist-get result :_meta)
                                 :io.modelcontextprotocol/serverInfo)))
    (if (not (member mcp--protocol-version versions))
        (mcp--connect-failed connection
                             mcp--unsupported-protocol-version
                             (format "server supports %s, client needs %s"
                                     (if versions
                                         (string-join versions ", ")
                                       "no protocol version")
                                     mcp--protocol-version))
      (setf (mcp--capabilities connection) capabilities
            (mcp--server-info connection) server-info
            (mcp--instructions connection) (plist-get result :instructions)
            ;; Connected before the callbacks run: they read the status.
            (mcp--status connection) 'connected)
      (message "[mcp] Connected! Server `MCP (%s)' now managing."
               (jsonrpc-name connection))
      (when (mcp--initial-callback connection)
        (funcall (mcp--initial-callback connection) connection))
      (mcp--fetch-capabilities connection syncp))))

(defun mcp--fetch-capabilities (connection syncp)
  "Fetch the lists advertised by the capabilities of CONNECTION."
  (let ((capabilities (mcp--capabilities connection)))
    (when (plist-member capabilities :prompts)
      (funcall (if syncp #'mcp-sync-list-prompts #'mcp-async-list-prompts)
               connection
               (mcp--prompts-callback connection)))
    (when (plist-member capabilities :tools)
      (funcall (if syncp #'mcp-sync-list-tools #'mcp-async-list-tools)
               connection
               (mcp--tools-callback connection)))
    (when (plist-member capabilities :resources)
      (funcall (if syncp #'mcp-sync-list-resources #'mcp-async-list-resources)
               connection
               (mcp--resources-callback connection))
      (funcall (if syncp #'mcp-sync-list-resource-templates #'mcp-async-list-resource-templates)
               connection
               (mcp--resources-templates-callback connection)))))

(defun mcp--start-connection (connection syncp)
  "Discover what CONNECTION supports, then load its lists.
SYNCP selects the synchronous request path."
  (mcp--discover connection
                 syncp
                 (lambda (result)
                   (mcp--on-discovered connection result syncp))
                 (lambda (code message data)
                   (mcp--connect-failed connection code message data))))

(defun mcp--server-running-p (name)
  "Return non-nil if server NAME is in running state."
  (when-let* ((conn (gethash name mcp-server-connections)))
    (not (member (mcp--status conn) '(stop error)))))

;;;###autoload
(cl-defun mcp-connect-server (name &key command args url env token headers roots
                                   timeout
                                   initial-callback tools-callback prompts-callback
                                   resources-callback resources-templates-callback
                                   error-callback syncp)
  "Connect to an MCP server with NAME, COMMAND, and ARGS or URL.

NAME is a string representing the name of the server.
COMMAND is a string representing the command to start the server
in stdio mcp server.
ARGS is a list of arguments to pass to the COMMAND.
URL is a string arguments to connect sse mcp server.
ENV is a plist argument to set mcp server env.

TOKEN is a string.
Authentication token used when connecting to an HTTP MCP server.

HEADERS is a alist.
Additional HTTP headers to include when connecting via URL.

ROOTS is a list of directory paths or root specifications.
Each root can be either:
- A string path (e.g., \"/home/user/project\")
- A plist with :uri and :name
  (e.g., (:uri \"file:///home/user/project\" :name \"Project\"))
Roots define filesystem boundaries that the server can access.

TIMEOUT is the timeout in seconds for jsonrpc async requests.
Defaults to `jsonrpc-default-request-timeout' when nil.

INITIAL-CALLBACK is a function called when the server completes
the connection.
TOOLS-CALLBACK is a function called to handle the list of tools
provided by the server.
PROMPTS-CALLBACK is a function called to handle the list of prompts
provided by the server.
RESOURCES-CALLBACK is a function called to handle the list of
resources provided by the server.
RESOURCES-TEMPLATES-CALLBACK is a function called to handle the list of
resources-templates provided by the server.
ERROR-CALLBACK is a function to call on error.
SYNCP specifies if the operation should be synchronous or asynchronous.

This function creates a new process for the server, initializes a connection,
and sends an initialization message to the server.  The connection is stored
in the `mcp-server-connections` hash table for future reference."
  (unless (mcp--server-running-p name)
    (when-let* ((server-config (cond (command
                                      (list :connection-type 'stdio
                                            :command command
                                            :args args))
                                     (url
                                      (when-let* ((res (mcp--parse-http-url url)))
                                        (plist-put res :connection-type 'http)
                                        (plist-put res :token token)
                                        (plist-put res :headers headers)))))
                (connection-type (plist-get server-config :connection-type))
                (buffer-name (format "*Mcp %s server*" name))
                (process-name (format "mcp-%s-server" name))
                (process (pcase connection-type
                           ('http 'empty)
                           ('stdio
                            (let ((env (mapcar (lambda (item)
                                                 (pcase-let* ((`(,key ,value) item))
                                                   (let ((key (symbol-name key)))
                                                     (list (substring key 1)
                                                           (format "%s" value)))))
                                               (seq-partition env 2)))
                                  (process-environment (copy-sequence process-environment)))
                              (when env
                                (dolist (elem env)
                                  (setenv (car elem) (cadr elem))))
                              (make-process
                               :name name
                               :command (append (list command)
                                                (plist-get server-config :args))
                               :connection-type 'pipe
                               :coding 'utf-8-unix
                               ;; :noquery t
                               :stderr (get-buffer-create
                                        (format "*%s stderr*" name))
                               ;; :file-handler t
                               ))))))
         (let ((connection (apply #'make-instance
                                `(,(pcase connection-type
                                     ('http
                                      'mcp-http-process-connection)
                                     ('stdio
                                      'mcp-stdio-process-connection))
                                  :connection-type ,connection-type
                                  :name ,name
                                  :process ,process
                                  :roots ,roots
                                  :timeout ,(or timeout jsonrpc-default-request-timeout)
                                  :events-buffer-config (:size ,mcp-log-size)
                                  :request-dispatcher ,(lambda (_ method params)
                                                         (funcall #'mcp--reject-server-request
                                                                  name method params))
                                  :notification-dispatcher ,(lambda (connection method params)
                                                              (funcall #'mcp-notification-dispatcher connection name method params))
                                  :on-shutdown ,(lambda (_)
                                                  (funcall #'mcp-on-shutdown name))
                                  :initial-callback ,initial-callback
                                  :prompts-callback ,prompts-callback
                                  :tools-callback ,tools-callback
                                  :resources-callback ,resources-callback
                                  :resources-templates-callback ,resources-templates-callback
                                  :error-callback ,error-callback
                                  ,@(when (equal connection-type 'http)
                                      (list :host (plist-get server-config :host)
                                            :port (plist-get server-config :port)
                                            :tls (plist-get server-config :tls)
                                            :path (plist-get server-config :path)
                                            :token (plist-get server-config :token)
                                            :headers (plist-get server-config :headers)))))))
        (puthash name connection mcp-server-connections)
        ;; Discover what the server supports, then load its lists.
        (let* ((msg-fn (lambda ()
                         (condition-case-unless-debug err
                             (if (jsonrpc-running-p connection)
                                 (mcp--start-connection connection syncp)
                               (error "Process start error"))
                           (error
                            (mcp-stop-server (jsonrpc-name connection))
                            (setf (mcp--status connection) 'error)
                            (when error-callback
                              (funcall error-callback -1 (format "%s" (cdr err))))
                            (message "Sadly, %s mcp server process start error" name))))))
          (if (not syncp)
              (run-with-idle-timer 1 nil msg-fn)
            (sit-for 1)
            (funcall msg-fn)))))))

;;;###autoload
(defun mcp-stop-server (name)
  "Stop the MCP server with the given NAME.
If the server is running, it will be shutdown and its connection will be removed
from `mcp-server-connections'. If no server with the given NAME is found,
a message will be displayed indicating that the server is not running."
  (if-let* ((connection (gethash name mcp-server-connections)))
      (progn
        (ignore-errors
          (jsonrpc-shutdown connection))
        (setf (mcp--status connection) 'stop))
    (message "mcp %s server not started" name)))

(defun mcp--parse-tool-args (properties required)
  "Parse tool arguments from PROPERTIES and REQUIRED lists.

PROPERTIES is a plist of tool argument properties.
REQUIRED is a list of required argument names.

The function processes each argument in PROPERTIES, marking optional arguments
if they are not in REQUIRED. Each argument is parsed into a structured plist
with :name, :type, and :optional fields.

Returns a list of parsed argument plists."
  (let ((need-length (- (/ (length properties) 2)
                        (length required))))
    (cl-mapcar (lambda (arg-value required-name)
                 (pcase-let* ((`(,key ,value) arg-value))
                   `( :name ,(substring (symbol-name key) 1)
                      ,@value
                      ,@(unless required-name
                          `(:optional t)))))
               (seq-partition properties 2)
               (append required
                       (when (> need-length 0)
                         (make-list need-length nil))))))


(defun mcp--parse-tool-call-result (res)
  "Parse the result of a tool call from RES.

RES is a plist representing the tool call result.

The function extracts text content from the result, concatenating it into
a single string if multiple text entries are present.

Returns the concatenated text or nil if no text content is found."
  (when (mcp--input-required-p res)
    (signal 'mcp-input-required
            "tool call returned an interim result that asked for input"))
  (string-join
   (cl-remove-if #'null
                 (mapcar (lambda (content)
                           (when (string= "text" (plist-get content :type))
                             (plist-get content :text)))
                         (plist-get res :content)))
   "\n"))

(defun mcp--generate-tool-call-args (args properties)
  "Generate tool call arguments from ARGS and PROPERTIES.

ARGS is a list of argument values provided by the caller.
PROPERTIES is a plist of tool argument properties.

The function matches ARGS to PROPERTIES, filling in default values for missing
optional arguments. It ensures the generated arguments match the tool's schema.

Returns a plist of argument names and values ready for tool invocation."
  (let ((need-length (- (/ (length properties) 2)
                        (length args))))
    (apply #'append
           (cl-mapcar (lambda (arg value)
                        (when-let* ((value (if value
                                               value
                                             (plist-get (cl-second arg)
                                                        :default))))
                          (list (cl-first arg)
                                value)))
                      (seq-partition properties 2)
                      (append args
                              (when (> need-length 0)
                                (make-list need-length nil)))))))

;;; Multi round-trip requests

;; A server may answer tools/call, prompts/get or resources/read with
;; resultType "input_required" instead of a result.  The client gathers
;; the requested input, then repeats the request with the answers and the
;; opaque requestState echoed back.  Every retry is a new JSON-RPC request:
;; nothing about the original call survives on the server.

(defconst mcp--mrtr-max-rounds 8
  "How many times one request may answer server input requests.")

;; Mirrors the server-side MissingRequiredClientCapability code.  It labels
;; the error handed to `mcp-connect-server' style callbacks, not a message
;; sent to the server.
(defconst mcp--input-required-code -32021
  "Error code reported when the client cannot provide requested input.")

(define-error 'mcp-input-required
  "MCP server requires client input that this client cannot provide")

(defun mcp--input-required-p (result)
  "Return non-nil when RESULT asks for more client input.
Results from earlier revisions carry no `resultType' and mean complete."
  (equal (plist-get result :resultType) "input_required"))

(defun mcp--mrtr-responses (roots input-requests)
  "Return responses for the INPUT-REQUESTS of a multi round-trip request.
ROOTS holds the client roots to offer.  INPUT-REQUESTS maps
server-chosen keys to request objects.  Signal `mcp-input-required' for a
request this client cannot answer, which is only possible for
capabilities it never declared."
  (let (responses)
    (cl-loop for (key request) on input-requests by #'cddr
             do (let ((method (plist-get request :method)))
                  (pcase method
                    ("roots/list"
                     (setq responses (append responses
                                             (list key (mcp--roots-result roots)))))
                    (_ (signal 'mcp-input-required
                               (format "server asked for %s, which this client cannot provide"
                                       method))))))
    responses))

(defun mcp--mrtr-retry-params (roots params result)
  "Return PARAMS extended with the answers RESULT asks for.
ROOTS holds the client roots to offer."
  (let ((requests (plist-get result :inputRequests))
        (state (plist-get result :requestState)))
    (append (mcp--params-to-plist params)
            (when requests
              (list :inputResponses (mcp--mrtr-responses roots requests)))
            ;; The server's opaque state must come back untouched, and must
            ;; not appear when the server sent none.
            (when state
              (list :requestState state)))))

(cl-defun mcp--request-with-input (connection method params)
  "Send METHOD to CONNECTION with PARAMS, answering server input requests.
Return the final complete result."
  (let ((round 0))
    (while t
      (let ((result (jsonrpc-request connection method params
                                     :timeout (mcp--timeout connection))))
        (unless (mcp--input-required-p result)
          (cl-return-from mcp--request-with-input result))
        (setq round (1+ round))
        (when (> round mcp--mrtr-max-rounds)
          (signal 'mcp-input-required
                  (format "%s asked for input %d times" method round)))
        (setq params (mcp--mrtr-retry-params (mcp--roots connection) params result))))))

(defun mcp--request-with-input-async (connection method params callback error-callback)
  "Send METHOD to CONNECTION with PARAMS asynchronously.
CALLBACK receives the final result.  ERROR-CALLBACK receives a code and
a message, including when the requested input cannot be provided."
  (cl-labels ((send (params round)
                (jsonrpc-async-request
                 connection method params
                 :timeout (mcp--timeout connection)
                 :success-fn
                 (lambda (result)
                   (if (not (mcp--input-required-p result))
                       (funcall callback result)
                     (condition-case err
                         (if (>= round mcp--mrtr-max-rounds)
                             (signal 'mcp-input-required
                                     (format "%s asked for input %d times"
                                             method (1+ round)))
                           (send (mcp--mrtr-retry-params (mcp--roots connection) params result)
                                 (1+ round)))
                       (mcp-input-required
                        (funcall error-callback mcp--input-required-code
                                 (error-message-string err))))))
                 :error-fn
                 (jsonrpc-lambda (&key code message _data)
                   (funcall error-callback code message)))))
    (send params 0)))

;;;###autoload
(defun mcp-make-text-tool (name tool-name &optional asyncp)
  "Create a `gptel' tool with the given NAME, TOOL-NAME, and ASYNCP.

NAME is the name of the server connection.
TOOL-NAME is the name of the tool to be created.

Currently, only synchronous messages are supported.

This function retrieves the tool definition from the server connection,
constructs a basic tool with the appropriate properties, and returns it.
The tool is configured to handle input arguments, call the server, and process
the response to extract and return text content."
  (when-let* ((connection (gethash name mcp-server-connections))
              (tools (mcp--tools connection))
              (tool (cl-find tool-name tools :test #'equal :key (lambda (tool) (plist-get tool :name)))))
    (cl-destructuring-bind (&key description ((:inputSchema input-schema)) &allow-other-keys) tool
      (cl-destructuring-bind (&key properties required &allow-other-keys) input-schema
        (list
         :function (if asyncp
                       (lambda (callback &rest args)
                         (when (< (length args) (length required))
                           (error "Error: args not match: %s -> %s" required args))
                         (if-let* ((connection (gethash name mcp-server-connections)))
                             (mcp-async-call-tool connection
                                                  tool-name
                                                  (mcp--generate-tool-call-args args properties)
                                                  (lambda (res)
                                                    (funcall callback
                                                             (mcp--parse-tool-call-result res)))
                                                  (lambda (code message)
                                                    (funcall callback
                                                             (format "call %s tool error with %s: %s"
                                                                     tool-name
                                                                     code
                                                                     message))))
                           (error "Error: %s server not connect" name)))
                     (lambda (&rest args)
                       (when (< (length args) (length required))
                         (error "Error: args not match: %s -> %s" required args))
                       (if-let* ((connection (gethash name mcp-server-connections)))
                           (if-let* ((res (mcp-call-tool connection
                                                         tool-name
                                                         (mcp--generate-tool-call-args args properties))))
                               (mcp--parse-tool-call-result res)
                             (error "Error: call %s tool error" tool-name))
                         (error "Error: %s server not connect" name))))
         :name tool-name
         :async asyncp
         :description description
         :args
         (mcp--parse-tool-args properties (or required '())))))))

(defconst mcp--list-max-pages 100
  "Upper bound on pages read while following list cursors.")

(defun mcp--list-cache-get (connection method)
  "Return the list cache entry of CONNECTION for METHOD, or nil."
  (gethash method (mcp--list-cache connection)))

(defun mcp--list-cache-put (connection method items ttl-ms)
  "Remember ITEMS for METHOD on CONNECTION for TTL-MS milliseconds.
A missing, zero or negative ttl makes the entry immediately stale."
  (puthash method
           (list :items items
                 :expires-at (when (and (numberp ttl-ms)
                                        (> ttl-ms 0))
                               (time-add (current-time)
                                         (seconds-to-time (/ ttl-ms 1000.0)))))
           (mcp--list-cache connection)))

(defun mcp--list-cache-fresh-p (entry)
  "Return non-nil when cache ENTRY is still fresh."
  (and entry
       (plist-get entry :expires-at)
       (time-less-p (current-time) (plist-get entry :expires-at))))

(defun mcp--list-pages (connection method key-name)
  "Return (ITEMS . TTL-MS) for the complete list of METHOD.
Follow every nextCursor, because the connection cache holds whole lists.
TTL-MS is the smallest positive page ttl, or nil when no page declared
one, since the assembled list is only as fresh as its weakest page."
  (let ((items [])
        (cursor nil)
        (ttl-ms nil)
        (pages 0))
    (catch 'done
      (while t
        (setq pages (1+ pages))
        (when (> pages mcp--list-max-pages)
          (message "[mcp] %s returned more than %d pages, stopping"
                   method mcp--list-max-pages)
          (throw 'done nil))
        (let* ((params (when cursor (list :cursor cursor)))
               (result (jsonrpc-request connection method params
                                        :timeout (mcp--timeout connection)))
               (page-ttl (plist-get result :ttlMs)))
          (setq items (vconcat items (append (plist-get result key-name) nil)))
          (when (and (numberp page-ttl) (> page-ttl 0))
            (setq ttl-ms (if ttl-ms (min ttl-ms page-ttl) page-ttl)))
          (setq cursor (plist-get result :nextCursor))
          (unless (and cursor (not (string-empty-p cursor)))
            (throw 'done nil)))))
    (cons items ttl-ms)))

(defun mcp--list-pages-async (connection method key-name done error-callback)
  "Fetch every page of METHOD, then call DONE with ITEMS and TTL-MS.
ERROR-CALLBACK receives a code and a message."
  (cl-labels ((page (cursor items ttl-ms pages)
                (jsonrpc-async-request
                 connection method (when cursor (list :cursor cursor))
                 :timeout (mcp--timeout connection)
                 :success-fn
                 (lambda (result)
                   (let* ((page-ttl (plist-get result :ttlMs))
                          (items (vconcat items (append (plist-get result key-name) nil)))
                          (ttl-ms (if (and (numberp page-ttl) (> page-ttl 0))
                                      (if ttl-ms (min ttl-ms page-ttl) page-ttl)
                                    ttl-ms))
                          (next (plist-get result :nextCursor)))
                     (cond
                      ((and next (not (string-empty-p next)))
                       (if (> pages mcp--list-max-pages)
                           (progn
                             (message "[mcp] %s returned more than %d pages, stopping"
                                      method mcp--list-max-pages)
                             (funcall done items ttl-ms))
                         (page next items ttl-ms (1+ pages))))
                      (t (funcall done items ttl-ms)))))
                 :error-fn
                 (jsonrpc-lambda (&key code message _data)
                   (funcall error-callback code message)))))
    (page nil [] nil 1)))

(defun mcp--list-items (connection method key-name slot-name callback error-callback syncp)
  "Load the complete list for METHOD from CONNECTION.

Follows cursors so the cached list is complete, and answers from the
cache while the server-declared ttlMs stays fresh.  KEY-NAME names the
result field, SLOT-NAME the connection slot that mirrors it.  CALLBACK
receives CONNECTION and the items.  ERROR-CALLBACK receives a code and a
message.  SYNCP selects the synchronous path.

Return the cached or fetched items, or nil when the request failed."
  (let* ((cached (and mcp-cache-lists (mcp--list-cache-get connection method)))
         (cached-items (plist-get cached :items)))
    (if (mcp--list-cache-fresh-p cached)
        (progn
          (setf (slot-value connection slot-name) cached-items)
          (when callback
            (funcall callback connection cached-items))
          cached-items)
      (let* ((store (lambda (items ttl-ms)
                      (setf (slot-value connection slot-name) items)
                      (mcp--list-cache-put connection method items ttl-ms)
                      (when callback
                        (funcall callback connection items))
                      items))
             (error-fn (jsonrpc-lambda (&key code message _data)
                         (if error-callback
                             (funcall error-callback code message)
                           (message "Sadly, %s mpc server reports %s: %s"
                                    (jsonrpc-name connection) code message)))))
        (if syncp
            (condition-case-unless-debug err
                (let ((pages (mcp--list-pages connection method key-name)))
                  (funcall store (car pages) (cdr pages)))
              (jsonrpc-error
               (funcall error-fn
                        (list :code (alist-get 'jsonrpc-error-code (cdr err))
                              :message (alist-get 'jsonrpc-error-message (cdr err))))
               nil))
          (mcp--list-pages-async connection method key-name store error-fn)
          cached-items)))))

(defun mcp-async-list-tools (connection &optional callback error-callback)
  "Get a list of tools from the MCP server using the provided CONNECTION.
CALLBACK is a function to call with the result.
ERROR-CALLBACK is a function to call on error."
  (mcp--list-items connection :tools/list :tools '-tools callback error-callback nil))

(defun mcp-sync-list-tools (connection &optional callback error-callback)
  "Get a list of tools from the MCP server using the provided CONNECTION.
CALLBACK is a function to call with the result.
ERROR-CALLBACK is a function to call on error."
  (mcp--list-items connection :tools/list :tools '-tools callback error-callback 'sync))

(defun mcp-call-tool (connection name arguments)
  "Call a tool on the remote CONNECTION with NAME and ARGUMENTS.

CONNECTION is the MCP connection object.
NAME is the name of the tool to call.
ARGGUMENTS is a list of arguments to pass to the tool."
  (mcp--request-with-input connection
                           :tools/call
                           (list :name name
                                 :arguments (if arguments
                                                arguments
                                              #s(hash-table)))))

(defun mcp-async-call-tool (connection name arguments callback error-callback)
  "Async Call a tool on the remote CONNECTION with NAME and ARGUMENTS.

CONNECTION is the MCP connection object.
NAME is the name of the tool to call.
ARGUMENTS is a list of arguments to pass to the tool.
CALLBACK is a function to call on success.
ERROR-CALLBACK is a function to call on error."
  (mcp--request-with-input-async connection
                                 :tools/call
                                 (list :name name
                                       :arguments (if arguments
                                                      arguments
                                                    #s(hash-table)))
                                 callback
                                 error-callback))

(defun mcp-async-list-prompts (connection &optional callback error-callback)
  "Get list of prompts from the MCP server using the provided CONNECTION.
CALLBACK is a function to call with the result.
ERROR-CALLBACK is a function to call on error."
  (mcp--list-items connection :prompts/list :prompts '-prompts callback error-callback nil))

(defun mcp-sync-list-prompts (connection &optional callback error-callback)
  "Get list of prompts from the MCP server using the provided CONNECTION.
CALLBACK is a function to call with the result.
ERROR-CALLBACK is a function to call on error."
  (mcp--list-items connection :prompts/list :prompts '-prompts callback error-callback 'sync))

(defun mcp-get-prompt (connection name arguments)
  "Call a prompt on the remote CONNECTION with NAME and ARGUMENTS.

CONNECTION is the MCP connection object.
NAME is the name of the prompt to call.
ARGGUMENTS is a list of arguments to pass to the prompt"
  (mcp--request-with-input connection
                           :prompts/get
                           (list :name name
                                 :arguments (if arguments
                                                arguments
                                              #s(hash-table)))))

(defun mcp-async-get-prompt (connection name arguments callback error-callback)
  "Async Call a prompt on the remote CONNECTION with NAME and ARGUMENTS.

CONNECTION is the MCP connection object.
NAME is the name of the prompt to call.
ARGUMENTS is a list of arguments to pass to the prompt.
CALLBACK is a function to call on successful response.
ERROR-CALLBACK is a function to call on error."
  (mcp--request-with-input-async connection
                                 :prompts/get
                                 (list :name name
                                       :arguments (if arguments
                                                      arguments
                                                    #s(hash-table)))
                                 callback
                                 error-callback))

(defun mcp-async-list-resources (connection &optional callback error-callback)
  "Get list of resources from the MCP server using the provided CONNECTION.
CALLBACK is a function to call with the result.
ERROR-CALLBACK is a function to call on error."
  (mcp--list-items connection :resources/list :resources '-resources callback error-callback nil))

(defun mcp-sync-list-resources (connection &optional callback error-callback)
  "Get list of resources from the MCP server using the provided CONNECTION.
CALLBACK is a function to call with the result.
ERROR-CALLBACK is a function to call on error."
  (mcp--list-items connection :resources/list :resources '-resources callback error-callback 'sync))

(defun mcp-read-resource (connection uri)
  "Call a resource on the remote CONNECTION with URI.

CONNECTION is the MCP connection object.
URI is the uri of the resource to call."
  (mcp--request-with-input connection
                           :resources/read
                           (list :uri uri)))

(defun mcp-async-read-resource (connection uri &optional callback error-callback)
  "Call a resource on the remote CONNECTION with URI.

CONNECTION is the MCP connection object.
URI is the URI of the resource to call.
CALLBACK is a function to call with the result on success.
ERROR-CALLBACK is a function to call with the error code and message on failure.

This function asynchronously reads a resource from the remote connection
using the specified URI. The result is passed to CALLBACK if the request
succeeds, or ERROR-CALLBACK if it fails."
  (mcp--request-with-input-async connection
                                 :resources/read
                                 (list :uri uri)
                                 callback
                                 error-callback))

(defun mcp-async-list-resource-templates (connection &optional callback error-callback)
  "Get list of resource templates from the MCP server using the CONNECTION.
CALLBACK is a function to call with the result.
ERROR-CALLBACK is a function to call on error."
  (mcp--list-items connection :resources/templates/list :resourceTemplates '-template-resources callback error-callback nil))

(defun mcp-sync-list-resource-templates (connection &optional callback error-callback)
  "Get list of resource templates from the MCP server using the CONNECTION.
CALLBACK is a function to call with the result.
ERROR-CALLBACK is a function to call on error."
  (mcp--list-items connection :resources/templates/list :resourceTemplates '-template-resources callback error-callback 'sync))

(defun mcp-set-roots (name roots)
  "Set the roots for MCP server NAME to ROOTS.

NAME is the server name (string).
ROOTS is a list of directory paths or root specifications.
Each root can be either:
- A string path (e.g., \"/home/user/project\")
- A plist with :uri and :name
  (e.g., (:uri \"file:///home/user/project\" :name \"Project\"))

The list stays local until a server asks for it, which the modern
protocol does through a multi round-trip request."
  (when-let* ((connection (gethash name mcp-server-connections)))
    (setf (mcp--roots connection) roots)))

(defun mcp-add-root (name root)
  "Add a ROOT to MCP server NAME.

NAME is the server name (string).
ROOT can be either:
- A string path (e.g., \"/home/user/project\")
- A plist with :uri and :name
  (e.g., (:uri \"file:///home/user/project\" :name \"Project\"))

The list stays local until a server asks for it."
  (when-let* ((connection (gethash name mcp-server-connections)))
    (let ((current-roots (mcp--roots connection)))
      (unless (member root current-roots)
        (setf (mcp--roots connection) (append current-roots (list root)))))))

(defun mcp-remove-root (name root)
  "Remove ROOT from MCP server NAME.

NAME is the server name (string).
ROOT is the root to remove (string path or plist).

The list stays local until a server asks for it."
  (when-let* ((connection (gethash name mcp-server-connections)))
    (setf (mcp--roots connection)
          (cl-remove root (mcp--roots connection) :test #'equal))))

(defun mcp-get-roots (name)
  "Get the current roots for MCP server NAME.

Returns a list of roots (strings or plists)."
  (when-let* ((connection (gethash name mcp-server-connections)))
    (condition-case nil
        (mcp--roots connection)
      (error nil))))

(provide 'mcp)
;;; mcp.el ends here
