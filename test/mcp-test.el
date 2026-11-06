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
