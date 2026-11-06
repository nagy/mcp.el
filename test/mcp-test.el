;;; mcp-test.el --- Tests for mcp.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

;; Unit tests only: no network access and no subprocesses.  They cover the
;; request metadata, the HTTP response readers and the multi round-trip
;; helper functions, which are the parts that encode protocol rules.

;;; Code:

(require 'ert)
(require 'mcp)

(defun mcp-test--json (value)
  "Serialize VALUE the way the transport does."
  (json-serialize value
                  :false-object :json-false
                  :null-object :json-null))

(defun mcp-test--count-key (key plist)
  "Count PLIST keys equal to KEY."
  (cl-loop for (k _v) on plist by #'cddr
           count (equal k key)))

;;;; Request metadata

(ert-deftest mcp-test-request-params-empty ()
  "A request without parameters still carries the protocol metadata."
  (let ((json (mcp-test--json (mcp--request-params nil))))
    (should (string-match-p
             "\"io.modelcontextprotocol/protocolVersion\":\"2026-07-28\"" json))
    (should (string-match-p
             "\"io.modelcontextprotocol/clientCapabilities\":{\"roots\":{}}" json))
    (should (string-match-p "\"name\":\"mcp-emacs\"" json))))

(ert-deftest mcp-test-request-params-plist ()
  "Caller parameters survive next to the metadata."
  (let ((params (mcp--request-params (list :name "search"
                                           :arguments (list :q "emacs")))))
    (should (equal (plist-get params :name) "search"))
    (should (equal (plist-get params :arguments) (list :q "emacs")))
    (should (plist-get params :_meta))))

(ert-deftest mcp-test-request-params-hash-table ()
  "Hash table parameters convert to a plist before the merge."
  (let ((table (make-hash-table :test #'equal)))
    (puthash :uri "file:///tmp/x" table)
    (let ((params (mcp--request-params table)))
      (should (equal (plist-get params :uri) "file:///tmp/x"))
      (should (plist-get params :_meta)))))

(ert-deftest mcp-test-request-meta-keeps-caller-keys ()
  "Unknown caller metadata survives, reserved keys do not duplicate."
  (let ((meta (mcp--request-meta
               (list :progressToken 7
                     :io.modelcontextprotocol/protocolVersion "1999-01-01"))))
    (should (equal (plist-get meta :progressToken) 7))
    (should (equal (plist-get meta :io.modelcontextprotocol/protocolVersion)
                   "2026-07-28"))
    (should (= 1 (mcp-test--count-key
                  :io.modelcontextprotocol/protocolVersion meta)))))

(ert-deftest mcp-test-request-meta-declares-roots ()
  "The client advertises the only capability it can answer: roots."
  (let* ((meta (mcp--request-meta))
         (capabilities (plist-get meta :io.modelcontextprotocol/clientCapabilities)))
    (should (hash-table-p capabilities))
    (should (hash-table-p (gethash "roots" capabilities)))))

;;;; HTTP response readers

(ert-deftest mcp-test-sse-payloads-multiple-events ()
  "Every event contributes a payload, not only the last one."
  (should (equal (mcp--sse-payloads
                  "event: message\ndata: {\"a\":1}\n\ndata: {\"b\":2}\n\n")
                 '("{\"a\":1}" "{\"b\":2}"))))

(ert-deftest mcp-test-sse-payloads-comments-and-crlf ()
  "Comments and CRLF line endings do not leak into payloads."
  (should (equal (mcp--sse-payloads ": ping\r\n\r\ndata: x\r\n\r\n")
                 '("x"))))

(ert-deftest mcp-test-sse-payloads-multiline-data ()
  "Consecutive data lines join with a newline, as SSE requires."
  (should (equal (mcp--sse-payloads "data: a\ndata: b\n\n") '("a\nb"))))

(ert-deftest mcp-test-sse-payloads-unterminated-event ()
  "A stream may end without a final blank line."
  (should (equal (mcp--sse-payloads "data: x") '("x"))))

(ert-deftest mcp-test-response-payloads-json ()
  "JSON bodies pass through, parameters after the type are ignored."
  (should (equal (mcp--response-payloads "application/json; charset=utf-8" "{}")
                 '("{}"))))

(ert-deftest mcp-test-response-payloads-event-stream ()
  "Event stream bodies go through the SSE reader."
  (should (equal (mcp--response-payloads "text/event-stream" "data: {}\n\n")
                 '("{}"))))

(ert-deftest mcp-test-response-payloads-unknown ()
  "An unknown content type carries no JSON-RPC message."
  (should-not (mcp--response-payloads "text/plain" "hi")))

(ert-deftest mcp-test-parse-json-message ()
  "Valid JSON parses to a plist, invalid JSON returns nil."
  (should (equal (plist-get (mcp--parse-json-message "{\"id\":1}") :id) 1))
  (cl-letf (((symbol-function 'jsonrpc--warn) #'ignore))
    (should-not (mcp--parse-json-message "{\"id\":"))))

;;;; Multi round-trip requests

(ert-deftest mcp-test-input-required-p ()
  "Only an explicit input_required result is interim."
  (should (mcp--input-required-p '(:resultType "input_required")))
  (should-not (mcp--input-required-p '(:resultType "complete")))
  (should-not (mcp--input-required-p '(:content [(:type "text" :text "hi")]))))

(ert-deftest mcp-test-mrtr-retry-params-echoes-state ()
  "The retry echoes requestState and adds nothing else when no input was asked."
  (let ((params (mcp--mrtr-retry-params
                 nil
                 (list :name "x" :arguments (list :a 1))
                 (list :resultType "input_required" :requestState "state"))))
    (should (equal (plist-get params :name) "x"))
    (should (equal (plist-get params :arguments) (list :a 1)))
    (should (equal (plist-get params :requestState) "state"))
    (should-not (plist-member params :inputResponses))))

(ert-deftest mcp-test-mrtr-retry-params-omits-absent-state ()
  "A server that sent no requestState must not receive one."
  (let ((params (mcp--mrtr-retry-params
                 nil
                 (list :uri "file:///x")
                 (list :resultType "input_required"))))
    (should-not (plist-member params :requestState))
    (should-not (plist-member params :inputResponses))))

(ert-deftest mcp-test-mrtr-responses-roots ()
  "A roots request is answered from the local roots list."
  (let* ((requests (list :r1 (list :method "roots/list" :params (make-hash-table))))
         (responses (mcp--mrtr-responses (list "/tmp") requests))
         (roots (plist-get (plist-get responses :r1) :roots)))
    (should (equal (plist-get responses :r1) (mcp--roots-result (list "/tmp"))))
    (should (= 1 (length roots)))))

(ert-deftest mcp-test-mrtr-responses-unsupported ()
  "An undeclared capability raises mcp-input-required."
  (should (eq t (condition-case nil
                    (progn (mcp--mrtr-responses
                            nil
                            (list :e1 (list :method "elicitation/create"
                                            :params (make-hash-table))))
                           nil)
                  (mcp-input-required t)))))

(ert-deftest mcp-test-roots-result-empty ()
  "No roots means an empty JSON array, not null."
  (let ((json (mcp-test--json (mcp--roots-result nil))))
    (should (equal json "{\"roots\":[]}"))))

(ert-deftest mcp-test-roots-result-string ()
  "Directory strings become file URIs with their base name."
  (let* ((result (mcp--roots-result (list "/tmp")))
         (root (aref (plist-get result :roots) 0)))
    (should (equal (plist-get root :uri) "file:///tmp"))
    (should (equal (plist-get root :name) "tmp"))))

;;;; Tool results

(ert-deftest mcp-test-tool-result-text ()
  "Text content joins into one string."
  (should (equal (mcp--parse-tool-call-result
                  (list :resultType "complete"
                        :content [(:type "text" :text "a")
                                  (:type "text" :text "b")]))
                 "a\nb")))

(ert-deftest mcp-test-tool-result-structured-only ()
  "A structured result without text becomes JSON, not an empty string."
  (should (equal (mcp--parse-tool-call-result
                  (list :resultType "complete"
                        :structuredContent (list :rows [1 2])))
                 "{\"rows\":[1,2]}")))

(ert-deftest mcp-test-tool-result-text-wins ()
  "Text content takes precedence over structured content."
  (should (equal (mcp--parse-tool-call-result
                  (list :resultType "complete"
                        :content [(:type "text" :text "shown")]
                        :structuredContent (list :rows [1])))
                 "shown")))

(ert-deftest mcp-test-tool-result-interim ()
  "An interim result raises instead of rendering as nothing."
  (should (eq t (condition-case nil
                    (progn (mcp--parse-tool-call-result
                            (list :resultType "input_required"))
                           nil)
                  (mcp-input-required t)))))

;;;; Pagination and list caching

(defun mcp-test--connection ()
  "Return a connection that needs no process, for pure tests."
  (make-instance 'mcp-http-process-connection
                 :name "test"
                 :connection-type 'http
                 :process 'empty
                 :host "127.0.0.1"
                 :port 1
                 :tls nil
                 :path "/mcp"
                 :token nil
                 :headers nil
                 :timeout 1))

(ert-deftest mcp-test-list-pages-follows-cursors ()
  "Every page contributes items, and the smallest ttl wins."
  (let ((conn (mcp-test--connection))
        (calls nil))
    (cl-letf (((symbol-function 'jsonrpc-request)
               (lambda (_connection _method params &rest _args)
                 (push params calls)
                 (if (= 1 (length calls))
                     (list :resultType "complete"
                           :tools [1 2]
                           :nextCursor "c1"
                           :ttlMs 5000)
                   (list :resultType "complete"
                         :tools [3]
                         :ttlMs 1000)))))
      (let ((pages (mcp--list-pages conn :tools/list :tools)))
        (should (equal (append (car pages) nil) '(1 2 3)))
        (should (= (cdr pages) 1000))
        (should (equal (nreverse calls) '(nil (:cursor "c1"))))))))

(ert-deftest mcp-test-list-pages-stops-without-cursor ()
  "A single page without a cursor ends the walk."
  (let ((conn (mcp-test--connection)))
    (cl-letf (((symbol-function 'jsonrpc-request)
               (lambda (&rest _args)
                 (list :resultType "complete" :prompts [1]))))
      (let ((pages (mcp--list-pages conn :prompts/list :prompts)))
        (should (equal (append (car pages) nil) '(1)))
        (should-not (cdr pages))))))

(ert-deftest mcp-test-list-cache-freshness ()
  "Only a positive ttl keeps a list entry fresh."
  (let ((conn (mcp-test--connection)))
    (mcp--list-cache-put conn :tools/list [1] 5000)
    (should (mcp--list-cache-fresh-p (mcp--list-cache-get conn :tools/list)))
    (dolist (ttl '(0 -5 nil))
      (mcp--list-cache-put conn :tools/list [1] ttl)
      (should-not (mcp--list-cache-fresh-p (mcp--list-cache-get conn :tools/list))))))

(ert-deftest mcp-test-list-items-serves-fresh-cache ()
  "A fresh list answers without touching the server."
  (let ((conn (mcp-test--connection)))
    (mcp--list-cache-put conn :tools/list [(:name "cached")] 10000)
    (cl-letf (((symbol-function 'jsonrpc-request)
               (lambda (&rest _args)
                 (ert-fail "a fresh cache must not reach the server"))))
      (let ((items (mcp--list-items conn :tools/list :tools '-tools nil nil t)))
        (should (equal (append items nil) '((:name "cached"))))
        (should (equal (append (mcp--tools conn) nil) '((:name "cached"))))))))

;;;; HTTP request headers

(defun mcp-test--tool (name schema)
  "Return a tool definition called NAME with SCHEMA."
  (list :name name :description name :inputSchema schema))

(defun mcp-test--schema (&rest properties)
  "Return an object schema with PROPERTIES."
  (list :type "object" :properties (append properties nil)))

(ert-deftest mcp-test-header-value-plain ()
  "Header-safe values travel verbatim."
  (should (equal (mcp--header-value "us-west1") "us-west1"))
  (should (equal (mcp--header-value "with space") "with space")))

(ert-deftest mcp-test-header-value-non-ascii ()
  "Non-ASCII values travel as base64 between the markers."
  (should (equal (mcp--header-value "Hello, 世界")
                 "=?base64?SGVsbG8sIOS4lueVjA==?=")))

(ert-deftest mcp-test-header-value-sentinel-shaped ()
  "A value that looks like an encoded value is encoded too."
  (should (equal (mcp--header-value "=?base64?literal?=")
                 "=?base64?PT9iYXNlNjQ/bGl0ZXJhbD89?=")))

(ert-deftest mcp-test-valid-header-name ()
  "Field names follow token syntax."
  (should (mcp--valid-header-name-p "Region"))
  (should (mcp--valid-header-name-p "x-tenant_id.v2"))
  (should-not (mcp--valid-header-name-p ""))
  (should-not (mcp--valid-header-name-p "has space"))
  (should-not (mcp--valid-header-name-p "colon:name")))

(ert-deftest mcp-test-tool-header-fields-valid ()
  "Annotations reachable through properties are accepted."
  (let* ((schema (mcp-test--schema
                  :region (list :type "string" :x-mcp-header "Region")
                  :query (list :type "string")))
         (fields (mcp--tool-header-fields (mcp-test--tool "sql" schema))))
    (should (equal fields '(((:region) . "Region"))))))

(ert-deftest mcp-test-tool-header-fields-nested ()
  "Nested object properties stay statically reachable."
  (let* ((schema (mcp-test--schema
                  :outer (list :type "object"
                               :properties (list :inner (list :type "string"
                                                              :x-mcp-header "Inner")))))
         (fields (mcp--tool-header-fields (mcp-test--tool "t" schema))))
    (should (equal fields '(((:outer :inner) . "Inner"))))))

(ert-deftest mcp-test-tool-header-fields-unreachable ()
  "An annotation under items is invalid, so the tool is invalid."
  (let ((schema (mcp-test--schema
                 :rows (list :type "array"
                             :items (list :type "object"
                                          :properties (list :id (list :type "string"
                                                                      :x-mcp-header "Id")))))))
    (should (eq t (condition-case nil
                      (progn (mcp--tool-header-fields (mcp-test--tool "t" schema)) nil)
                    (mcp-invalid-tool t))))))

(ert-deftest mcp-test-tool-header-fields-bad-type ()
  "Only string, boolean and integer parameters may become headers."
  (let ((schema (mcp-test--schema
                 :ratio (list :type "number" :x-mcp-header "Ratio"))))
    (should (eq t (condition-case nil
                      (progn (mcp--tool-header-fields (mcp-test--tool "t" schema)) nil)
                    (mcp-invalid-tool t))))))

(ert-deftest mcp-test-tool-header-fields-duplicate ()
  "Two parameters may not share a header name, ignoring case."
  (let ((schema (mcp-test--schema
                 :a (list :type "string" :x-mcp-header "Same")
                 :b (list :type "integer" :x-mcp-header "same"))))
    (should (eq t (condition-case nil
                      (progn (mcp--tool-header-fields (mcp-test--tool "t" schema)) nil)
                    (mcp-invalid-tool t))))))

(ert-deftest mcp-test-validated-tools-drops-invalid ()
  "Invalid definitions are dropped, valid ones survive."
  (let* ((good (mcp-test--tool "good" (mcp-test--schema :r (list :type "string"))))
         (bad (mcp-test--tool "bad" (mcp-test--schema
                                     :n (list :type "number" :x-mcp-header "N"))))
         (kept (cl-letf (((symbol-function 'message) #'ignore))
                 (mcp--validated-tools (vector good bad)))))
    (should (= 1 (length kept)))
    (should (equal (plist-get (aref kept 0) :name) "good"))))

(ert-deftest mcp-test-http-headers ()
  "Standard and parameter headers mirror the request body."
  (let* ((conn (mcp-test--connection))
         (tool (mcp-test--tool "execute_sql"
                               (mcp-test--schema
                                :region (list :type "string" :x-mcp-header "Region"))))
         (_ (setf (mcp--tools conn) (vector tool)))
         (headers (mcp--http-headers
                   conn
                   (list :method "tools/call"
                         :params (list :name "execute_sql"
                                       :arguments (list :region "us-west1")))))
         (get (lambda (name) (cdr (assoc name headers)))))
    (should (equal (funcall get "Mcp-Method") "tools/call"))
    (should (equal (funcall get "Mcp-Name") "execute_sql"))
    (should (equal (funcall get "Mcp-Param-Region") "us-west1"))
    (should (equal (funcall get "MCP-Protocol-Version") "2026-07-28"))))

(ert-deftest mcp-test-http-headers-omit-unsafe-integer ()
  "An integer JavaScript cannot represent is not mirrored."
  (let* ((conn (mcp-test--connection))
         (tool (mcp-test--tool "scaled"
                               (mcp-test--schema
                                :weight (list :type "integer" :x-mcp-header "Weight")))))
    (setf (mcp--tools conn) (vector tool))
    (let ((headers (cl-letf (((symbol-function 'message) #'ignore))
                     (mcp--http-headers
                      conn
                      (list :method "tools/call"
                            :params (list :name "scaled"
                                          :arguments (list :weight 9007199254740992)))))))
      (should-not (assoc "Mcp-Param-Weight" headers)))))

(ert-deftest mcp-test-http-headers-omit-missing-param ()
  "A parameter with no value produces no header."
  (let* ((conn (mcp-test--connection))
         (tool (mcp-test--tool "execute_sql"
                               (mcp-test--schema
                                :region (list :type "string" :x-mcp-header "Region")))))
    (setf (mcp--tools conn) (vector tool))
    (let ((headers (mcp--http-headers
                    conn
                    (list :method "tools/call"
                          :params (list :name "execute_sql" :arguments (list))))))
      (should-not (assoc "Mcp-Param-Region" headers)))))

;;;; Cancellation

(ert-deftest mcp-test-cancel-http-reports-error ()
  "Cancelling an HTTP request closes its stream and reports locally."
  (let ((conn (mcp-test--connection))
        (captured nil))
    (mcp--http-stream-put conn 7 (list :connection conn :phase 'body))
    (cl-letf (((symbol-function 'jsonrpc-connection-receive)
               (lambda (_connection message) (push message captured))))
      (mcp-cancel-request conn 7 "user quit"))
    (should (eq (mcp--http-stream-get conn 7) :finished))
    (should (equal (plist-get (plist-get (car captured) :error) :code) -32800))
    (should (equal (plist-get (plist-get (car captured) :error) :message)
                   "user quit"))))

(ert-deftest mcp-test-cancel-request-handle-follows-id ()
  "A handle cancels the attempt that is in flight now."
  (let* ((conn (mcp-test--connection))
         (handle (mcp--make-request conn 3))
         (captured nil))
    (mcp--http-stream-put conn 3 (list :connection conn :phase 'body))
    (cl-letf (((symbol-function 'jsonrpc-connection-receive)
               (lambda (_connection message) (push message captured))))
      (setf (mcp-request-id handle) 4)
      (mcp--http-stream-put conn 4 (list :connection conn :phase 'body))
      (mcp-cancel-request nil handle "user quit"))
    (should (eq (mcp--http-stream-get conn 4) :finished))
    (should (equal (mcp--http-stream-get conn 3)
                   (list :connection conn :phase 'body)))
    (should (equal (plist-get (car captured) :id) 4))))

(ert-deftest mcp-test-cancel-stdio-notifies ()
  "Cancelling a stdio request sends notifications/cancelled."
  (let ((conn (mcp-test--connection))
        (sent nil)
        (captured nil))
    (setf (mcp--connection-type conn) 'stdio)
    (cl-letf (((symbol-function 'mcp-notify)
               (lambda (_connection method params)
                 (push (cons method params) sent)))
              ((symbol-function 'jsonrpc-connection-receive)
               (lambda (_connection message) (push message captured))))
      (mcp-cancel-request conn 9))
    (should (equal (caar sent) :notifications/cancelled))
    (should (equal (plist-get (cdar sent) :requestId) 9))
    (should (equal (plist-get (plist-get (car captured) :error) :code) -32800))))

;;;; Stream table keying and late output

(ert-deftest mcp-test-streams-isolated-per-connection ()
  "Two connections may use the same request id at the same time."
  (let ((a (mcp-test--connection))
        (b (mcp-test--connection))
        (delivered nil))
    (cl-letf (((symbol-function 'mcp--http-deliver)
               (lambda (_connection _id payload)
                 (push payload delivered)
                 (not (null payload)))))
      (mcp--http-consume a 1 nil "data: {\"from\":\"a\"}\n\n")
      (mcp--http-consume b 1 nil "data: {\"from\":\"b\"}\n\n"))
    (should (equal (plist-get (mcp--http-stream-get a 1) :pending) ""))
    (should (equal (plist-get (mcp--http-stream-get b 1) :pending) ""))
    (should (eq (mcp--http-stream-get a 1)
                (mcp--http-stream-get a 1)))
    (should-not (eq (mcp--http-stream-get a 1)
                    (mcp--http-stream-get b 1)))))

(ert-deftest mcp-test-list-cache-is-per-connection ()
  "One connection never answers from another connection's list cache."
  (let ((a (mcp-test--connection))
        (b (mcp-test--connection)))
    (should-not (eq (mcp--list-cache a) (mcp--list-cache b)))
    (mcp--list-cache-put a :tools/list [(:name "a-only")] 60000)
    (should (mcp--list-cache-get a :tools/list))
    (should-not (mcp--list-cache-get b :tools/list))))

(ert-deftest mcp-test-consume-ignores-finished-stream ()
  "Output that arrives after the stream closed does not break the filter."
  (let ((conn (mcp-test--connection)))
    (mcp--http-stream-put conn 1 :finished)
    (should (eq (mcp--http-consume conn 1 nil "HTTP/1.1 200 OK\r\n\r\n{}")
                :finished))
    (should (eq (mcp--http-stream-get conn 1) :finished))))

(ert-deftest mcp-test-cancel-leaves-other-connection-alone ()
  "Cancelling an id on one connection never closes another's stream."
  (let ((a (mcp-test--connection))
        (b (mcp-test--connection)))
    (mcp--http-stream-put a 1 (list :phase 'body))
    (mcp--http-stream-put b 1 (list :phase 'body))
    (cl-letf (((symbol-function 'jsonrpc-connection-receive) #'ignore))
      (mcp-cancel-request b 1))
    (should (eq (mcp--http-stream-get b 1) :finished))
    (should (equal (mcp--http-stream-get a 1) (list :phase 'body)))))

;;;; HTTP headers that follow the JSON body

(ert-deftest mcp-test-header-scalar-booleans ()
  "A boolean header spells the JSON value, not the Lisp one."
  (should (equal (mcp--header-scalar t) "true"))
  (should (equal (mcp--header-scalar :json-false) "false"))
  (should-not (mcp--header-scalar nil))
  (should (equal (mcp--header-scalar 42) "42"))
  (should (equal (mcp--header-scalar "us-west1") "us-west1")))

(defun mcp-test--decode-header (value)
  "Return the decoded text of header VALUE."
  (if (and (string-prefix-p "=?base64?" value)
           (string-suffix-p "?=" value))
      (decode-coding-string
       (base64-decode-string (substring value 9 (- (length value) 2)))
       'utf-8)
    value))

(ert-deftest mcp-test-header-value-quotes ()
  "A value holding a quote or a backslash stays out of the curl config."
  (dolist (value '("say \"hi\"" "back\\slash" "=?base64?x?="))
    (let ((encoded (mcp--header-value value)))
      (should (string-prefix-p "=?base64?" encoded))
      (should (equal (mcp-test--decode-header encoded) value))))
  (should (equal (mcp--header-value "plain-value") "plain-value")))

(ert-deftest mcp-test-http-headers-boolean-parameter ()
  "A false parameter mirrors as false, like the body says."
  (let* ((conn (mcp-test--connection))
         (tool (mcp-test--tool "flag"
                               (mcp-test--schema
                                :on (list :type "boolean" :x-mcp-header "On")))))
    (setf (mcp--tools conn) (vector tool))
    (let* ((headers (mcp--http-headers
                     conn
                     (list :method "tools/call"
                           :params (list :name "flag"
                                         :arguments (list :on :json-false)))))
           (value (cdr (assoc "Mcp-Param-On" headers))))
      (should (equal value "false")))))

(ert-deftest mcp-test-http-headers-reply-has-no-method ()
  "A reply carries no method, so it gets no Mcp-Method header."
  (let* ((conn (mcp-test--connection))
         (headers (mcp--http-headers
                   conn
                   (list :id 3 :error (list :code -32601 :message "no")))))
    (should-not (assoc "Mcp-Method" headers))
    (should-not (assoc "Mcp-Name" headers))
    (should (assoc "MCP-Protocol-Version" headers))))

;;;; Response payloads with a sloppy content type

(ert-deftest mcp-test-response-payloads-missing-content-type ()
  "A JSON body counts as JSON even without the content type."
  (should (equal (mcp--response-payloads nil "{\"jsonrpc\":\"2.0\"}")
                 '("{\"jsonrpc\":\"2.0\"}")))
  (should (equal (mcp--response-payloads "text/plain" "{\"ok\":true}")
                 '("{\"ok\":true}")))
  (should-not (mcp--response-payloads "text/html" "<html></html>"))
  (should-not (mcp--response-payloads nil "")))

;;;; Multi round-trip retries

(ert-deftest mcp-test-mrtr-retry-params-single-keys ()
  "A retry built twice still carries one value per key."
  (let* ((result (list :inputRequests (list :r1 (list :method "roots/list"))
                       :requestState "state-1"))
         (first (mcp--mrtr-retry-params '("/tmp") '(:name "t") result))
         (second (mcp--mrtr-retry-params '("/tmp") first result)))
    (dolist (params (list first second))
      (should (= 1 (mcp-test--count-key :inputResponses params)))
      (should (= 1 (mcp-test--count-key :requestState params)))
      (should (= 1 (mcp-test--count-key :name params))))
    (should (equal (plist-get second :requestState) "state-1"))))

(ert-deftest mcp-test-request-with-input-retries-cleanly ()
  "Every round sends the answers once, built from the caller params."
  (let ((conn (mcp-test--connection))
        (sent nil)
        (round 0))
    (setf (mcp--roots conn) '("/tmp/root"))
    (cl-letf (((symbol-function 'jsonrpc-request)
               (lambda (_connection _method params &rest _args)
                 (push params sent)
                 (setq round (1+ round))
                 (if (< round 3)
                     (list :resultType "input_required"
                           :inputRequests (list :r1 (list :method "roots/list"))
                           :requestState (format "state-%d" round))
                   (list :resultType "complete")))))
      (let ((result (mcp--request-with-input conn :tools/call '(:name "roots"))))
        (should (equal (plist-get result :resultType) "complete"))))
    (dolist (params sent)
      (should (= 1 (mcp-test--count-key :inputResponses params)))
      (should (= 1 (mcp-test--count-key :requestState params))))
    (should (equal (plist-get (plist-get (car sent) :inputResponses) :r1)
                   (mcp--roots-result '("/tmp/root"))))
    (should (equal (plist-get (car sent) :requestState) "state-2"))))

;;;; Discover timeouts

(ert-deftest mcp-test-discover-timeout-reports-error ()
  "A timeout reaches the error callback with all three arguments."
  (let ((conn (mcp-test--connection))
        (timeout-fn nil)
        (reported nil))
    (cl-letf (((symbol-function 'jsonrpc-async-request)
               (lambda (&rest args)
                 ;; ARGS starts with the connection, so pair up from the tail.
                 (setq timeout-fn
                       (cl-loop for (key value) on (cdr args) by #'cddr
                                when (eq key :timeout-fn) return value))
                 '(1 nil))))
      (mcp--discover conn nil #'ignore
                     (lambda (code message data)
                       (setq reported (list code message data))))
      (should (functionp timeout-fn))
      (funcall timeout-fn))
    (should (equal reported '(124 "timeout" nil)))))

;;;; Error helpers

(ert-deftest mcp-test-supported-versions-of ()
  "The supported list of an UnsupportedProtocolVersionError is readable."
  (should (equal (mcp--supported-versions-of
                  (list :requested "2026-07-28"
                        :supported ["2025-11-25"]))
                 '("2025-11-25")))
  (should-not (mcp--supported-versions-of nil)))

(provide 'mcp-test)
;;; mcp-test.el ends here
