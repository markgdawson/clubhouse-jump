;;; shortcuttr.el --- Navigate projects on clubhouse.io -*- lexical-binding: t -*-

;; Copyright © 2021 Mark Dawson <markgdawson@gmail.com>

;; Author: Mark Dawson <bozhidar@batsov.com>
;; URL: https://github.com/markgdawson/clubhouse-jump
;; Keywords: project, convenience
;; Version: 0.0.1-snapshot
;; Package-Requires: ((emacs "25.1") (magit "2.0.0") (dash "1.0.0"))

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
;; This library provides utilities for working with projects on clubhouse.io
;;
;;; Code:

(require 'magit)
(require 'subr-x)
(require 'dash)

(defun shortcuttr-alist-completing-read-key (key prompt alist-candidates)
  "Return the value slot of an association in `alist-candidates` by
prompting for the key using `completing-read`. Keys in `alist-candidates` should
be strings."
  (let ((candidates (mapcar (lambda (alist) (cons (alist-get key alist) alist)) alist-candidates)))
    (alist-get (completing-read prompt candidates) candidates nil nil #'string-equal)))

;; -----------------------------------------
;; Implementation
;; -----------------------------------------

(defun shortcuttr-epic-stories (epic-id)
  (org-clubhouse--search-stories (format "epic:%d" epic-id)))

(defcustom shortcuttr-epics-query
  "project:\"Engineering\" state:\"in progress\""
  "Default clubhouse query to fetch epics.")

(defun shortcuttr-fetch-epics ()
  (-> (org-clubhouse-request "GET" "search/epics" :params `((query ,shortcuttr-epics-query)))
    cdadr
    (append nil)
    reject-archived))

(defun shortcuttr-completing-read-epic ()
  (shortcuttr-alist-completing-read-key 'name "Epic: " (shortcuttr-fetch-epics)))

(defun shortcuttr-completing-read-story-from-epic (epic-id)
  (shortcuttr-alist-completing-read-key 'name "Story: " (shortcuttr-epic-stories epic-id)))

(defun shortcuttr-completing-read-story ()
  (shortcuttr-completing-read-story-from-epic
   (alist-get 'id
              (shortcuttr-completing-read-epic))))

(defun shortcuttr-completing-read-story-url ()
  (alist-get 'app_url (shortcuttr-completing-read-story)))

(defun shortcuttr-jump-url ()
  "Open a clubhouse story in the default web browser."
  (interactive)
  (browse-url (shortcuttr-completing-read-story-url)))

(defun shortcuttr-url-as-kill ()
  "Copy a clubhouse URL as kill."
  (interactive)
  (let ((url (shortcuttr-completing-read-story-url)))
    (message url)
    (kill-new url)))

(defun shortcuttr-branch-name-guess (story)
  "Guess a sensible branch name for clubhouse STORY."
  (downcase
   (string-replace " " "-" (alist-get 'name story))))

(defun shortcuttr-prompt-for-label (story)
  "Prompt for a label for branch/MR linked to STORY with sensible defaults."
  (let* ((prefix (format "ch%s/"
                         (alist-get 'id story)))
         (title-guess (shortcuttr-branch-name-guess story))
         (branch-guess (concat prefix title-guess)))
    (read-string "Branch name: " branch-guess)))

(defun shortcuttr-magit-branch-create ()
  "Create a branch named after a clubhouse story."
  (interactive)
  (let ((branch (shortcuttr-prompt-for-label
                 (shortcuttr-completing-read-story))))
    (magit-branch-create
     branch
     (magit-read-starting-point "Starting from"))
    (magit-checkout branch)))

(provide 'shortcuttr)
;;; shortcuttr.el ends here
