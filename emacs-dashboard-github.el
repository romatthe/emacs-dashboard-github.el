;;; -*- lexical-binding: t -*-

(use-package request
  :straight t)

(use-package dash
  :straight t)

(use-package dashboard
  :straight t)

;;(require 'request)
;;(require 'dash)
;;(require 'dashboard)
;;(require 'dashboard-widgets)

(add-to-list 'dashboard-item-generators  '(github . dashboard-insert-github))
(add-to-list 'dashboard-items '(github) t)

(defconst dgh--supported-event-types'
  ("CreateEvent"
   "ForkEvent"
   "IssueCommentEvent"
   ;; "IssuesEvent"
   ;; "MemberEvent"
   ;; "MembershipEvent"
   ;; "PublicEvent"
   ;; "PullRequestEvent"
   ;; "PullRequestReviewEvent"
   ;; "PullRequestReviewCommentEvent"
   ;; "ReleaseEvent"
   ;; "RepositoryEvent"
   ;; "StarEvent"
   "WatchEvent")
  "Github API event types current supported by the package.")

(defconst dgh-test-create
  '((type  . "CreateEvent")
    (actor . ((login . "Joey")))
    (repo  . ((name . "example/example-repo")))))

(defconst dgh-test-fork
  '((type  . "ForkEvent")
    (actor . ((login . "Joey")))
    (payload . ((forkee . ((full_name . "Joey/example-repo")))))
    (repo  . ((name . "example/example-repo")))))

(defconst dgh-test-watch
  '((type  . "WatchEvent")
    (actor . ((login . "Joey")))
    (repo  . ((name . "example/example-repo")))))

(defvar dashboard-github-user-name "romatthe"
  "Github user name for which to fetch events.")

(defvar dashboard-github-user-events-url
  (format "https://api.github.com/users/%s/received_events/public"
	  dashboard-github-user-name)
  "Github public events API URL.")

(defun dgh--event-is-supported? (event)
  "Check if EVENT is one of the supported event types."
  (-contains? dgh--supported-event-types (assoc-default 'type event)))

(defun dgh--get-events ()
  "Get public Github events, and execute CALLBACK function."
  (let
      ((request-result
	(request
	 dashboard-github-user-events-url
	 :sync t
	 :parser 'json-read
	 :success (cl-function (lambda (&key data &allow-other-keys) ())))))
    (append (request-response-data request-result) nil)))
	
(defun dgh--parse-events (events)
  "Parse EVENTS data from events request."
  (-map #'dgh--parse-event
	(-filter #'dgh--event-is-supported? events)))

(defun dgh--parse-event (event)
  "Parse a single EVENT."
  (pcase (assoc-default 'type event)
    ("CreateEvent"                   (dgh--parse-create-event event))
    ("ForkEvent"                     (dgh--parse-fork-event event))
    ("IssueCommentEvent"             (dashboard-github-parse-issue-comment-event event))
    ;; ("IssuesEvent"                   (message "IssuesEvent"))
    ;; ("MemberEvent"                   (message "MemberEvent"))
    ;; ("MembershipEvent"               (message "MembershipEvent"))
    ;; ("PublicEvent"                   (message "PublicEvent"))
    ;; ("PullRequestEvent"              (message "PullRequestEvent"))
    ;; ("PullRequestReviewEvent"        (message "PullRequestReviewEvent"))
    ;; ("PullRequestReviewCommentEvent" (message "PullRequestReviewCommentEvent"))
    ;; ("ReleaseEvent"                  (message "ReleaseEvent"))
    ;; ("RepositoryEvent"               (message "RepositoryEvent"))
    ;; ("StarEvent"                     (message "StarEvent"))
    ("WatchEvent"                     (dgh--parse-watch-event event))))

(defun dgh--parse-create-event (event)
  "Parse a CreateEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] created a repository [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (format "https://github.com/%s"
		      (assoc-recursive event 'repo 'name)))))

(defun dgh--parse-fork-event (event)
  "Parse a ForkEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] forked [%s] from [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'forkee 'full_name)
		      (assoc-recursive event 'repo 'name)))
    (cons 'url (format "https://github.com/%s"
		       (assoc-recursive event 'repo 'name)))))
 
(defun dgh--parse-watch-event (event)
  "Parse a WatchEvent data scructure from EVENT."
  (list
   (cons 'msg (format "[%s] starred [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (format "https://github.com/%s"
		      (assoc-recursive event 'repo 'name)))))

(defun dashboard-github-parse-issue-comment-event (event)
  "Parse an IssueCommentEvent data structure from EVENT."
  (list
   (cons 'msg (format "[%s] %s comment on [%s](#%d) in [%s]"
		      (assoc-recursive event 'actor 'login)
		      (assoc-recursive event 'payload 'action)
		      (assoc-recursive event 'payload 'issue 'title)
		      (assoc-recursive event 'payload 'issue 'number)
		      (assoc-recursive event 'repo 'name)))
   (cons 'url (assoc-recursive event 'payload 'comment 'html_url))))

(defun dashboard-insert-github (list-size)
  "Insert LIST-SIZE amount of events from Github into the dashboard."
 (dashboard-insert-section
   "Github:"
   (dgh--parse-events (dgh--get-events))
   list-size
   "g"
   (lambda (&rest ignore) (browse-url (assoc-default 'url el)))
   (assoc-default 'msg el)))

(defun assoc-recursive (alist &rest keys)
  "Recursively find (as KEYS) in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(provide 'emacs-dashboard-github)

;;; emacs-dashboard-github.el ends here
