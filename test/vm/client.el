;;; client.el --- Network scenario driver for mcp.el  -*- lexical-binding: t; -*-

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

;; Drives the client against test/vm/fake_server.py over real sockets and
;; real subprocesses, which the ERT suite cannot do.  The NixOS test runs
;; this inside a VM, so a transport regression fails a machine, not a mock.
;;
;; Environment:
;;   MCP_VM_URL_A, MCP_VM_URL_B     endpoint of each HTTP server
;;   MCP_VM_STATS_A, MCP_VM_STATS_B where each server reports what it saw
;;   MCP_VM_PYTHON, MCP_VM_FAKE     interpreter and script of the fake server
;;   MCP_VM_ROOT                    a directory to offer as a root
;;
;; Exit status is zero only when every check passed.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mcp)

(defvar mcp-vm--failures nil
  "Messages of the checks that failed.")

(defvar mcp-vm--notifications nil
  "Notification methods the client received, newest first.")

(defun mcp-vm--fail (message)
  "Record and print MESSAGE as a failed check."
  (push message mcp-vm--failures)
  (princ (format "FAIL: %s\n" message)))

(defun mcp-vm--check (value format-string &rest args)
  "Report a failed check unless VALUE holds."
  (unless value
    (mcp-vm--fail (apply #'format format-string args))))

(defun mcp-vm--check-equal (expected actual format-string &rest args)
  "Report a failed check unless ACTUAL equals EXPECTED."
  (unless (equal expected actual)
    (mcp-vm--fail (format "%s: expected %S, got %S"
                          (apply #'format format-string args)
                          expected actual))))

(defun mcp-vm--env (name)
  "Return the value of environment variable NAME, or signal."
  (or (getenv name)
      (error "Missing environment variable %s" name)))

(defun mcp-vm--wait (predicate &optional seconds)
  "Wait until PREDICATE returns non-nil, up to SECONDS."
  (let ((deadline (+ (float-time) (or seconds 20))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun mcp-vm--stats (url)
  "Return the stats of the fake server at URL as a hash table."
  (with-temp-buffer
    (let ((status (call-process "curl" nil t nil "-sf" url)))
      (unless (equal status 0)
        (error "curl failed for %s with %S" url status))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'hash-table :null-object nil
                         :false-object :json-false))))

(defun mcp-vm--stat (stats name)
  "Return the stat called NAME from STATS."
  (gethash name stats))

(defun mcp-vm--tool (name tools)
  "Return the tool called NAME from TOOLS, or nil."
  (cl-find name tools :test #'equal
           :key (lambda (tool) (plist-get tool :name))))

(defun mcp-vm--root-uri (root)
  "Return the URI the client offers for ROOT."
  (concat "file://" (expand-file-name root)))

(defun mcp-vm--connect (name url roots)
  "Connect server NAME at URL with ROOTS and return its state plist."
  (let ((state (list :connected nil :tools nil :error nil)))
    (mcp-connect-server name
                        :url url
                        :roots roots
                        :initial-callback
                        (lambda (connection)
                          (setf (plist-get state :connected) connection))
                        :tools-callback
                        (lambda (_connection items)
                          (setf (plist-get state :tools)
                                (append items nil)))
                        :error-callback
                        (lambda (code message)
                          (setf (plist-get state :error) (list code message))))
    state))

(defun mcp-vm--start-call (connection tool arguments)
  "Call TOOL on CONNECTION with ARGUMENTS and return a result plist.
The plist carries :done, :result, :error and the :handle to cancel."
  (let ((out (list :done nil :result nil :error nil :handle nil)))
    (setf (plist-get out :handle)
          (mcp-async-call-tool
           connection tool arguments
           (lambda (result)
             (setf (plist-get out :result) result
                   (plist-get out :done) t))
           (lambda (code message)
             (setf (plist-get out :error) (list :code code :message message)
                   (plist-get out :done) t))))
    out))

(defun mcp-vm--call (connection tool arguments)
  "Call TOOL on CONNECTION, wait, and return the finished result plist."
  (let ((out (mcp-vm--start-call connection tool arguments)))
    (mcp-vm--wait (lambda () (plist-get out :done)) 30)
    out))

(defun mcp-vm--text (result)
  "Return the text content of a tool RESULT, or nil."
  (let ((content (plist-get result :content)))
    (when (and content (> (length content) 0))
      (plist-get (aref content 0) :text))))

(defun mcp-vm--check-tool-text (out expected label)
  "Check that the call OUT answered with the text EXPECTED."
  (mcp-vm--check (plist-get out :done) "%s never finished" label)
  (mcp-vm--check (null (plist-get out :error)) "%s failed: %S" label
                 (plist-get out :error))
  (mcp-vm--check-equal expected (mcp-vm--text (plist-get out :result))
                       "%s answered" label))

(defun mcp-vm--two-servers ()
  "Connect two HTTP servers at once and check they stay apart."
  (let* ((url-a (mcp-vm--env "MCP_VM_URL_A"))
         (url-b (mcp-vm--env "MCP_VM_URL_B"))
         (root (mcp-vm--env "MCP_VM_ROOT"))
         (a (mcp-vm--connect "a" url-a (list root)))
         (b (mcp-vm--connect "b" url-b nil))
         (conn-a (gethash "a" mcp-server-connections))
         (conn-b (gethash "b" mcp-server-connections)))
    (mcp-vm--check (mcp-vm--wait (lambda () (and (plist-get a :tools)
                                                 (plist-get b :tools))))
                   "both servers never finished loading their tools")
    (mcp-vm--check (plist-get a :connected) "server a never connected")
    (mcp-vm--check (plist-get b :connected) "server b never connected")
    (dolist (entry (list (cons "a" a) (cons "b" b)))
      (let ((tools (plist-get (cdr entry) :tools)))
        (mcp-vm--check (mcp-vm--tool "after-two" tools)
                       "%s missed the second tools page" (car entry))
        (mcp-vm--check-equal 9 (length tools)
                             "%s tool count" (car entry))))
    (mcp-vm--check-equal 'connected (mcp--status conn-a) "server a status")
    (mcp-vm--check-equal 'connected (mcp--status conn-b) "server b status")
    ;; Concurrent calls must not answer each other's request.
    (let ((first (mcp-vm--start-call conn-a "echo" (list :text "one")))
          (second (mcp-vm--start-call conn-b "echo" (list :text "two"))))
      (mcp-vm--wait (lambda () (and (plist-get first :done)
                                    (plist-get second :done)))
                    30)
      (mcp-vm--check-tool-text first "echo[a]:one" "server a echo")
      (mcp-vm--check-tool-text second "echo[b]:two" "server b echo"))
    (list :conn-a conn-a :conn-b conn-b)))

(defun mcp-vm--roots-round-trip (conn)
  "Check that a roots answer travels back inside a retried tool call."
  (let ((out (mcp-vm--call conn "roots" nil)))
    (mcp-vm--check-tool-text
     out
     (format "roots[a]:%s" (mcp-vm--root-uri (mcp-vm--env "MCP_VM_ROOT")))
     "roots call")))

(defun mcp-vm--event-stream (conn)
  "Check that a request-scoped event stream delivers progress and a result."
  (let ((out (mcp-vm--call conn "stream" nil)))
    (mcp-vm--check (null (plist-get out :error))
                   "stream call failed: %S" (plist-get out :error))
    (let ((structured (plist-get (plist-get out :result) :structuredContent)))
      (mcp-vm--check-equal "stream[a]" (plist-get structured :from)
                           "stream structured content"))
    (mcp-vm--check-equal "{\"from\":\"stream[a]\"}"
                         (mcp--parse-tool-call-result
                          (plist-get out :result))
                         "structured result text")
    (mcp-vm--check (member 'notifications/progress mcp-vm--notifications)
                   "the progress notification never reached the client")))

(defun mcp-vm--headers (conn)
  "Check that names and parameter values mirror into request headers."
  (let ((sql (mcp-vm--call conn "sql" '(:region "us-west1" :query "select 1")))
        (flag (mcp-vm--call conn "flag" (list :on :json-false)))
        (utf8 (mcp-vm--call conn "utf8-世界" nil))
        (quoted (mcp-vm--call conn "quote\"name" nil)))
    (mcp-vm--check-tool-text sql "sql[a]:us-west1:select 1" "sql call")
    (mcp-vm--check-tool-text flag "flag[a]:false" "flag call")
    (mcp-vm--check-tool-text utf8 "utf8[a]" "non-ASCII tool name")
    (mcp-vm--check-tool-text quoted "quote[a]" "quoted tool name")))

(defun mcp-vm--cancellation (conn)
  "Check that cancelling an HTTP request answers the caller."
  (let ((out (mcp-vm--start-call conn "slow" nil)))
    (accept-process-output nil 0.5)
    (mcp-cancel-request conn (plist-get out :handle) "network test")
    (mcp-vm--check (mcp-vm--wait (lambda () (plist-get out :done)) 10)
                   "a cancelled call never reported back")
    (mcp-vm--check-equal -32800 (plist-get (plist-get out :error) :code)
                         "cancellation error code")))

(defun mcp-vm--cache (conn)
  "Check that a fresh list answers from the cache."
  (let ((out (list :done nil :items nil)))
    (mcp-async-list-tools
     conn
     (lambda (_connection items)
       (setf (plist-get out :items) (append items nil)
             (plist-get out :done) t)))
    (mcp-vm--wait (lambda () (plist-get out :done)) 20)
    (mcp-vm--check-equal 9 (length (plist-get out :items))
                         "cached tool count")))

(defun mcp-vm--sync-connection (url root)
  "Connect synchronously to URL with ROOT and check the round trips."
  (mcp-connect-server "sync" :url url :roots (list root) :syncp t)
  (let ((conn (gethash "sync" mcp-server-connections))
        (uri (mcp-vm--root-uri root)))
    (mcp-vm--check-equal 'connected (mcp--status conn) "sync status")
    (mcp-vm--check (mcp-vm--tool "after-two" (mcp--tools conn))
                   "sync client missed the second tools page")
    (mcp-vm--check-equal (format "roots[a]:%s" uri)
                         (mcp-vm--text (mcp-call-tool conn "roots" nil))
                         "sync roots call")
    (mcp-vm--check-equal "echo[a]:sync"
                         (mcp-vm--text (mcp-call-tool conn "echo"
                                                      (list :text "sync")))
                         "sync echo call")))

(defun mcp-vm--stdio (root)
  "Connect over stdio and check a tool call and a roots round trip."
  (let ((python (mcp-vm--env "MCP_VM_PYTHON"))
        (script (mcp-vm--env "MCP_VM_FAKE"))
        (uri (mcp-vm--root-uri root)))
    (mcp-connect-server "stdio"
                        :command python
                        :args (list script "--transport" "stdio"
                                    "--name" "stdio")
                        :roots (list root))
    (let ((conn (gethash "stdio" mcp-server-connections))
          (tools nil))
      (mcp-vm--check
       (mcp-vm--wait (lambda ()
                       (setq tools (mcp--tools conn))
                       (and (eq (mcp--status conn) 'connected) tools))
                     30)
       "the stdio server never connected")
      (mcp-vm--check (mcp-vm--tool "after-two" tools)
                     "stdio client missed the second tools page")
      (mcp-vm--check-tool-text (mcp-vm--call conn "echo" (list :text "stdio"))
                               "echo[stdio]:stdio" "stdio echo")
      (mcp-vm--check-tool-text (mcp-vm--call conn "roots" nil)
                               (format "roots[stdio]:%s" uri)
                               "stdio roots round trip"))))

(defun mcp-vm--check-stats (url label list-calls sse-calls)
  "Check what the fake server at URL recorded.
LIST-CALLS counts tools/list requests, SSE-CALLS event streams."
  (let* ((stats (mcp-vm--stats url))
         (violations (append (mcp-vm--stat stats "violations") nil))
         (counts (mcp-vm--stat stats "counts")))
    (mcp-vm--check (null violations) "%s reported violations: %S"
                   label violations)
    (mcp-vm--check-equal list-calls (mcp-vm--stat counts "tools/list")
                         "%s tools/list calls" label)
    (mcp-vm--check (>= (or (mcp-vm--stat counts "tools/call") 0) 1)
                   "%s saw no tools/call" label)
    (mcp-vm--check-equal sse-calls (mcp-vm--stat stats "sse")
                         "%s event streams" label)))

(defun mcp-vm--buffer-text (name)
  "Return the text of buffer NAME, or an empty string."
  (if (get-buffer name)
      (with-current-buffer (get-buffer name)
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun mcp-vm--check-log ()
  "Check that the client stayed quiet while talking to the servers."
  (let ((log (concat (mcp-vm--buffer-text "*Messages*")
                     (mcp-vm--buffer-text "*Warnings*"))))
    (dolist (needle '("Error running timer"
                      "error in process filter"
                      "Wrong type argument"
                      "wrong-number-of-arguments"
                      "no response from"
                      "Sadly"
                      "Invalid JSON"))
      (mcp-vm--check (not (string-search needle log))
                     "the client logged %S" needle))))

(defun mcp-vm--run ()
  "Run every scenario and return the list of failures."
  (let* ((url-a (mcp-vm--env "MCP_VM_URL_A"))
         (root (mcp-vm--env "MCP_VM_ROOT"))
         (servers (mcp-vm--two-servers))
         (conn-a (plist-get servers :conn-a)))
    (mcp-vm--cache conn-a)
    ;; The cache answered, so the server saw two pages only.
    (mcp-vm--sync-connection url-a root)
    (mcp-vm--roots-round-trip conn-a)
    (mcp-vm--event-stream conn-a)
    (mcp-vm--headers conn-a)
    (mcp-vm--cancellation conn-a)
    (mcp-vm--stdio root)
    ;; Server a served the sync connection too, so it listed twice more.
    (mcp-vm--check-stats (mcp-vm--env "MCP_VM_STATS_A") "server a" 4 1)
    (mcp-vm--check-stats (mcp-vm--env "MCP_VM_STATS_B") "server b" 2 0)
    (dolist (name '("a" "b" "sync" "stdio"))
      (ignore-errors (mcp-stop-server name)))
    (mcp-vm--check-log)
    (nreverse mcp-vm--failures)))

(advice-add 'mcp-notification-dispatcher :before
            (lambda (_connection _name method _params)
              (push method mcp-vm--notifications)))

(run-with-timer
 300 nil
 (lambda ()
   (princ "WATCHDOG: the network test hung\n")
   (kill-emacs 3)))

(condition-case err
    (let ((failures (mcp-vm--run)))
      (if failures
          (progn
            (princ (format "%d check(s) failed\n" (length failures)))
            (kill-emacs 1))
        (princ "PASS: every network check held\n")
        (kill-emacs 0)))
  (error
   (princ (format "ERROR: %s\n" (error-message-string err)))
   (kill-emacs 2)))

;;; client.el ends here
