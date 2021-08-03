;;; clubhouse-jump.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2021 Mark Dawson <markgdawson@gmail.com>

;; Author: Mark Dawson <bozhidar@batsov.com>
;; URL: https://github.com/markgdawson/clubhouse-jump
;; Keywords: project, convenience
;; Version: 0.0.1-snapshot
;; Package-Requires: ((emacs "25.1") org-clubhouse)

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;
;;; Code:

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

(provide 'clubhouse-jump)
