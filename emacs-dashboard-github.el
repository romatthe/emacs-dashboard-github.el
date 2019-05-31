;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(use-package request
  :straight t)

(defconst dashboard-github-user-events-url
  (format "https://api.github.com/users/%s/received_events/public"
	  "romatthe"))

(defun dashboard-github-get-events (callback)
  "Get public Github events, and execute CALLBACK function."
  (request
   dashboard-github-user-events-url
   :sync t
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
	       (funcall callback data)))))

(defun dashboard-github-parse-events (events)
  "Parse EVENTS data from events request."
  (mapcar (lambda (event) (dashboard-github-parse-event event)) events))


(defun dashboard-github-parse-event (event)
  "Parse single EVENT."
  )


(dashboard-github-get-events 'dashboard-github-parse-events)
