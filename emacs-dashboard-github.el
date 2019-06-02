;;; -*- lexical-binding: t -*-

(use-package request
  :straight t)

(use-package dash
  :straight t)

(defconst dgh--supported-event-types'
  ("CreateEvent"
   "ForkEvent"
   "IssueCommentEvent"
   "IssuesEvent"
   "MemberEvent"
   "MembershipEvent"
   "PublicEvent"
   "PullRequestEvent"
   "PullRequestReviewEvent"
   "PullRequestReviewCommentEvent"
   "ReleaseEvent"
   "RepositoryEvent"
   "StarEvent"
   "WatchEvent")
  "Github API event types current supported by the package.")

(defconst dgh-test-create
  '((type  . "CreateEvent")
    (actor . ((login . "Joey")))
    (repo  . ((name . "example/example-repo")
	      (url . "https://www.example.org")))))

(defconst dgh-test-fork
  '((type  . "ForkEvent")
    (actor . ((login . "Joey")))
    (payload . ((forkee . ((full_name . "Joey/example-repo")))))
    (repo  . ((name . "example/example-repo")
	      (url . "https://www.example.org")))))

(defvar dashboard-github-user-name "romatthe"
  "Github user name for which to fetch events.")

(defvar dashboard-github-user-events-url
  (format "https://api.github.com/users/%s/received_events/public"
	  dashboard-github-user-name)
  "Github public events API URL.")

(defun dgh--event-is-supported? (event)
  "Check if EVENT is one of the supported event types."
  (-contains? dgh--supported-event-types (assoc-default 'type event)))

(defun dgh--get-events (callback)
  "Get public Github events, and execute CALLBACK function."
  (request
   dashboard-github-user-events-url
   :sync t
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
	       (funcall callback data)))))

(defun dgh--parse-events (events)
  "Parse EVENTS data from events request."
  (-map 'dgh--parse-events
	(-filter 'dgh--event-is-supported? events)))

(defun dgh--parse-event (event)
  "Parse a single EVENT."
  (pcase (assoc-default 'type event)
    ("CreateEvent"                   (message (dgh--parse-create-event event)))
    ("ForkEvent"                     (message (dgh--parse-fork-event event)))
    ("IssueCommentEvent"             (message "IssueCommentEvent"))
    ("IssuesEvent"                   (message "IssuesEvent"))
    ("MemberEvent"                   (message "MemberEvent"))
    ("MembershipEvent"               (message "MembershipEvent"))
    ("PublicEvent"                   (message "PublicEvent"))
    ("PullRequestEvent"              (message "PullRequestEvent"))
    ("PullRequestReviewEvent"        (message "PullRequestReviewEvent"))
    ("PullRequestReviewCommentEvent" (message "PullRequestReviewCommentEvent"))
    ("ReleaseEvent"                  (message "ReleaseEvent"))
    ("RepositoryEvent"               (message "RepositoryEvent"))
    ("StarEvent"                     (message "StarEvent"))
    ("WatchEvent"                    (message "WatchEvent"))))

(defun dgh--parse-create-event (event)
  "Parse a CreateEvent data structure from EVENT."
  '((msg . (format "%s created a repository %s"
		   (assoc-recursive event 'actor 'login)
		   (assoc-recursive event 'repo 'name)))
    (url . (format "https://github.com/%s"
		   (assoc-recursive event 'repo 'name)))))

(defun dgh--parse-fork-event (event)
  "Parse a ForkCreateEvent data structure from EVENT."
  '((png . ?????)
    (msg . (format "%s forked %s from %s"
		   (assoc-recursive event 'actor 'login)
		   (assoc-recursive event 'payload 'forkee 'full_name)
		   (assoc-recursive event 'repo 'name)))
    (url . (format "https://github.com/%s"
		   (assoc-recursive event 'repo 'name)))))

(defun assoc-recursive (alist &rest keys)
  "Recursively find (as KEYS) in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(dgh--get-events 'dgh--parse-events)

(provide 'emacs-dashboard-github)

;;; emacs-dashboard-github.el ends here
