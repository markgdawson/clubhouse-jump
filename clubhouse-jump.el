;; -----------------------------------------
;; Copied Utility Functions
;; -----------------------------------------
(defun mgd/alist-completing-read (prompt alist-candidates)
  "Return the value slot of an association in `alist-candidates` by
prompting for the key using `completing-read`. Keys in `alist-candidates` should
be strings."
  (alist-get (completing-read prompt alist-candidates) alist-candidates nil nil #'string-equal))

(defun mgd/alist-completing-read-key (key prompt alist-candidates)
  "Return the value slot of an association in `alist-candidates` by
prompting for the key using `completing-read`. Keys in `alist-candidates` should
be strings."
  (let ((candidates (mapcar (lambda (alist) (cons (alist-get key alist) alist)) alist-candidates)))
    (alist-get (completing-read prompt candidates) candidates nil nil #'string-equal)))

;; -----------------------------------------
;; Implementation
;; -----------------------------------------

(defun epic-stories (epic-id)
  (org-clubhouse--search-stories (format "epic:%d" epic-id)))

(defcustom clubhouse-epics-query
  "project:\"Engineering\" state:\"in progress\""
  "Default clubhouse query to fetch epics.")

(defun fetch-epics ()
  (-> (org-clubhouse-request "GET" "search/epics" :params `((query ,clubhouse-epics-query)))
    cdadr
    (append nil)
    reject-archived))

(defun clubhouse-completing-read-epic ()
  (mgd/alist-completing-read-key 'name "Epic: " (fetch-epics)))

(defun clubhouse-completing-read-story (epic-id)
  (mgd/alist-completing-read-key 'name "Story: " (epic-stories epic-id)))


(defun clubhouse-completing-read-story-url ()
  (alist-get 'app_url
             (clubhouse-completing-read-story (alist-get 'id
                                                         (clubhouse-completing-read-epic)))))

;; -----------------------------------------
;; Interactive functions
;; -----------------------------------------

(defun clubhouse-jump ()
  "Open a clubhouse story in the default web browser."
  (interactive)
  (browse-url (clubhouse-completing-read-story-url)))

(defun clubhouse-url-as-kill ()
  "Copy a clubhouse URL as kill."
  (interactive)
  (let ((url (clubhouse-completing-read-story-url)))
    (message url)
    (kill-new url)))
