;;; -*- lexical-binding: t -*-

(require 'request)
(require 'dash)
(require 'dashboard)
(require 'dashboard-widgets)

(add-to-list 'dashboard-item-generators  '(github . dashboard-insert-github))
(add-to-list 'dashboard-items '(github) t)

(defconst dashboard-github-supported-event-types
  ("CreateEvent"
   "ForkEvent"
   "IssuesEvent"
   "IssueCommentEvent"
   "PublicEvent"
   "PullRequestEvent"
   "PullRequestReviewEvent"
   "PullRequestReviewCommentEvent"
   "ReleaseEvent"
   "WatchEvent")
  "Github API event types current supported by the package.")

(defvar dashboard-github-user-name "romatthe"
  "Github user name for which to fetch events.")

(defvar dashboard-github-user-events-url
  (format "https://api.github.com/users/%s/received_events/public"
	  dashboard-github-user-name)
  "Github public events API URL.")

(defun dashboard-github-event-is-supported? (event)
  "Check if EVENT is one of the supported event types."
  (-contains? dashboard-github-supported-event-types (assoc-default 'type event)))

(defun dashboard-github-get-events ()
  "Get public Github events, and execute CALLBACK function."
  (let
      ((request-result
	(request
	 dashboard-github-user-events-url
	 :sync t
	 :parser 'json-read
	 :success (cl-function (lambda (&key data &allow-other-keys) ())))))
    (append (request-response-data request-result) nil)))
	
(defun dashboard-github-parse-events (events)
  "Parse EVENTS data from events request."
  (-map #'dashboard-github-parse-event
	(-filter #'dashboard-github-event-is-supported? events)))

(defun dashboard-github-parse-event (event)
  "Parse a single EVENT."
  (pcase (assoc-default 'type event)
    ("CreateEvent"                   (dashboard-github-parse-create-event event))
    ("ForkEvent"                     (dashboard-github-parse-fork-event event))
    ("IssuesEvent"                   (dashboard-github-parse-issue-event event))
    ("IssueCommentEvent"             (dashboard-github-parse-issue-comment-event event))
    ("PublicEvent"                   (dashboard-github-parse-public-event event))
    ("PullRequestEvent"              (dashboard-github-parse-pull-request-event event))
    ("PullRequestReviewEvent"        (dashboard-github-parse-pull-request-review-event event))
    ("PullRequestReviewCommentEvent" (dashboard-github-parse-pull-request-review-comment-event event))
    ("ReleaseEvent"                  (dashboard-github-parse-release-event event))
    ("WatchEvent"                    (dashboard-github-parse-watch-event event))))

(defun dashboard-github-parse-create-event (event)
  "Parse a CreateEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] created a repository [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (format "https://github.com/%s"
		      (assoc-recursive event 'repo 'name)))))

(defun dashboard-github-parse-fork-event (event)
  "Parse a ForkEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] forked [%s] from [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'forkee 'full_name)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (format "https://github.com/%s"
		      (assoc-recursive event 'repo 'name)))))
 
(defun dashboard-github-parse-watch-event (event)
  "Parse a WatchEvent data scructure from EVENT."
  (list
   (cons 'msg (format "[%s] starred [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (format "https://github.com/%s"
		      (assoc-recursive event 'repo 'name)))))

(defun dashboard-github-parse-issue-event (event)
  "Parse an IssueEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] %s issue [%s](%d) in [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'action)
		      (assoc-recursive event 'payload 'issue 'title)
		      (assoc-recursive event 'payload 'issue 'number)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (assoc-recursive event 'payload 'issue 'html_url))))

(defun dashboard-github-parse-issue-comment-event (event)
  "Parse an IssueCommentEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] %s comment on issue [%s](#%d) in [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'action)
		      (assoc-recursive event 'payload 'issue 'title)
		      (assoc-recursive event 'payload 'issue 'number)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (assoc-recursive event 'payload 'comment 'html_url))))

(defun dashboard-github-public-event (event)
  "Parse a PublicEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] has made [%s] public"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'repository 'full_name)))
   (cons 'url (assoc-recursive event 'payload 'repository 'html_url))))

(defun dashboard-github-parse-pull-request-event (event)
  "Parse a PullRequestEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] %s PR [%s](#%d) in [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'action)
		      (assoc-recursive event 'payload 'pull_request 'title)
		      (assoc-recursive event 'payload 'pull_request 'number)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (assoc-recursive event 'payload 'pull_request 'html_url))))

(defun dashboard-github-parse-pull-request-review-event (event)
  "Parse a PullRequestReview data structure from EVENT."
  (list
   (cons 'msg (format "[%s] %s a PR review for [%s](#%d) in [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'action)
		      (assoc-recursive event 'payload 'pull_request 'title)
		      (assoc-recursive event 'payload 'pull_request 'number)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (assoc-recursive event 'payload 'review  'html_url))))

(defun dashboard-github-parse-pull-request-review-comment-event (event)
  "Parse a PullRequestReviewComment structure from EVENT."
  (list
   (cons 'msg (format "[%s] %s a comment on a PR review for [%s](#%d) in [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'action)
		      (assoc-recursive event 'payload 'pull_request 'title)
		      (assoc-recursive event 'payload 'pull_request 'number)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (assoc-recursive event 'payload 'comment 'html_url))))

(defun dashboard-github-parse-release-event (event)
  "Parse a ReleaseEvent structure from EVENT."
  (list
   (cons 'msg (format "[%s] %s release (%s) for [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'action)
		      (assoc-recursive event 'payload 'release 'tag_name)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (assoc-recursive event 'payload 'release 'html_url))))

(defun dashboard-insert-github (list-size)
  "Insert LIST-SIZE amount of events from Github into the dashboard."
 (dashboard-insert-section
   "Github:"
   (dashboard-github-parse-events (dashboard-github-get-events))
   list-size
   "g"
   (lambda (&rest ignore) (browse-url (assoc-default 'url el)))
   (assoc-default 'msg el)))

(defun assoc-recursive (alist &rest keys)
  "Recursively find (as KEYS) in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

;;; Loop structure
;;(defun find-it (loops)
;;  (catch 'enough
;;    (let ((total 0))  ; otherwise a total is a void variable
;;      (dotimes (number loops total)
;;	(setq total (+ total (1+ number)))
;;	(when (equal total 4) (throw 'enough total))))))

(provide 'emacs-dashboard-github)

;;; emacs-dashboard-github.el ends here
