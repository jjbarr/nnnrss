;;; nnnrss.el --- NEW nnrss for gnus, powered by nnfeed -*- lexical-binding:t -*-
;; Copyright (C) 2025 Joshua Barrett
;; Package-Requires: ((emacs "30.1")
;;                    (gnus "5.13"))
;; Author: Joshua Barrett <jjbarr@ptnote.dev>
;; Keywords: gnus rss
;; Version: 0.1
;; Url: https://github.com/jjbarr/nnnrss
;; Created: 8th Mar 2025
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;; 
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; The introduction of Emacs 30 has brought with it atom support for gnus, built
;; on top of a new generic feed backend called "nnfeed".  However, RSS has been
;; left on the old nnrss implementation, which mostly works fine... ish, but has
;; a number of awkward limitations and discontinuities compared to
;; nnatom/nnfeed.
;; 
;; Thus, nnnrss, the NEW nnrss, which rewrites nnrss to use nnfeed. 
;;; Code:

(require 'nnfeed)
(require 'mm-url)
(require 'dom)
(require 'cl-lib)
(require 'subr-x)
;; An apprciable portion of what follows is code adapted from nnatom, the nnfeed
;; examples, and the original nnrss.
(defgroup nnnrss nil
  "New RSS backend for Gnus."
  :group 'nnfeed)
(nnoo-declare nnnrss nnfeed)
(nnfeed-define-basic-backend-interface nnnrss)

(declare-function libxml-parse-xml-region "xml.c")
(declare-function libxml-parse-html-region "xml.c")
(defun nnnrss--read-feed (feed _)
  "Return a list structure representing FEED, or nil."
  (if (string-match-p "\\`https?://" feed)
      (nnheader-report
       nnnrss-backend
       "Addresses shouldn't start with \"https://\" or \"https://\""))
  (with-temp-buffer
    (condition-case e
        (if (file-name-absolute-p feed)
            (insert-file-contents feed)
          (mm-url-insert-file-contents (concat "https://" feed)))
      (file-error (nnheader-report nnnrss-backend (cdr e)))
      (:success
       (when-let ((data (if (libxml-available-p)
                            ;; rss is a bit.. well, it's not very picky.
                            (condition-case e
                                (libxml-parse-xml-region (point-min) (point-max))
                              (error (libxml-parse-html-region (point-min) (point-max))))
                          (car (xml-parse-region
                                (point-min) (point-max)))))
                  (authors (list 'authors)))
         (when (eq (car data) 'top)
           (setq data (assq 'rss data)))
         (setq data (assq 'channel data))
         (dom-add-child-before data authors)
         (let ((all (dom-children data)))
           (while-let ((rest (cdr all))
                       (child (car-safe rest))
                       (type (car-safe child))
                       ((not (eq type 'item))))
             (when (or (eq type 'managingEditor)
                       (eq type 'webMaster))
               ;;unlike atom, rss doesn't have a notion of anything other than
               ;;freeform text for authors
               (dom-add-child-before authors child))
             (setq all rest))
           (setcdr all (nreverse (cdr all))))
         data)))))

(defvoo nnnrss-read-feed-function #'nnnrss--read-feed
  nil nnfeed-read-feed-function)

(defun nnnrss--read-group (data)
  "Return the next group and remaining DATA, or nil."
  `(,data))
(defvoo nnnrss-read-group-function #'nnnrss--read-group
  nil nnfeed-read-group-function)

(defun nnnrss--read-article (data _)
  "Return the next article and the remaining DATA in a cons cell, or nil."
  (when (eq (car data) 'channel) (setq data (dom-children data)))
  (while (and data (not (eq (car-safe (car data)) 'item))) (pop data))
  (when-let ((article (car data))
             (authors (list 'authors))
             (links (list 'links)))
    (dom-add-child-before article links)
    (dom-add-child-before article authors)
    (dolist (child (cddddr article) `(,article . ,(cdr data)))
      (pcase (car-safe child)
        ('author (dom-add-child-before authors child))
        ((or 'link 'enclosure
             (and 'guid
                  (guard
                   (not (equal "false" (assq 'isPermaLink
                                             (dom-attributes child)))))))
         (dom-add-child-before links child))))))
(defvoo nnnrss-read-article-function #'nnnrss--read-article
  nil nnfeed-read-article-function)

(defun nnnrss--read-title (group)
  "Return the title of GROUP or nil."
  (when-let (title (dom-child-by-tag group 'title))
    (string-trim (dom-text title))))
(defvoo nnnrss-read-title-function #'nnnrss--read-title
  nil nnfeed-read-title-function)

(defun nnnrss--read-description (group)
  "Return the description of GROUP or nil."
  (when-let (desc (dom-child-by-tag group 'description))
    (string-trim (dom-text desc))))
(defvoo nnnrss-read-description-function #'nnnrss--read-description
  nil nnfeed-read-description-function)

;;; I frankly give up on the mess that is the RSS author field.
(defun nnnrss--read-article-or-group-authors (article-or-group)
  (when-let ((authors (mapconcat
                       (lambda (author) (string-trim (dom-text author)))
                       (dom-children (dom-child-by-tag article-or-group 'authors))
                       ", "))
             ((not (string-blank-p authors))))
    authors))
(defvoo nnnrss-read-group-author-function #'nnnrss--read-article-or-group-authors
  nil nnfeed-read-group-author-function)
(defvoo nnnrss-read-author-function #'nnnrss--read-article-or-group-authors
  nil nnfeed-read-author-function)

(defun nnnrss--read-id (article)
  (when-let (id (dom-child-by-tag article 'guid))
    (string-trim (dom-text id))))
(defvoo nnnrss-read-id-function #'nnnrss--read-id
  nil nnfeed-read-id-function)

(defun nnnrss--read-subject (article)
  (when-let (subject (dom-child-by-tag article 'title))
    (string-trim (dom-text subject))))
(defvoo nnnrss-read-subject-function #'nnnrss--read-subject
  nil nnfeed-read-subject-function)

(defun nnnrss--read-publish-date (article)
  (when-let (date (dom-child-by-tag article 'pubDate))
    (date-to-time (string-trim (dom-text date)))))
(defvoo nnnrss-read-publish-date-function #'nnnrss--read-publish-date
  nil nnfeed-read-publish-date-function)

(defun nnnrss--read-links (article)
  (let ((links 0) (enc 0))
    (mapcan
     (lambda (link)
       (pcase (car-safe link)
         ((or 'link 'guid)
          (let ((l (string-trim (dom-text link)))
                (lab (format "Link %s" (cl-incf links))))
            `(((("text/html")  . ,(format "<a href=\"%s\">[%s]</a><br/>" l lab))
               (("text/plain") . ,(format "%s: %s\n" lab l))))))
         ('enclosure
          (let* ((l (dom-attr link 'url))
                 (cty (dom-attr link 'type))
                 (lab (format "Enclosure %s (%s)" (cl-incf enc) cty)))
            `(((("text/html") . ,(format "<a href=\"%s\">[%s]</a><br/>" l lab))
               (("text/plain") . ,(format "%s: %s" lab l))))))
         (_ ())))
     (dom-children (dom-child-by-tag article 'links)))))
(defvoo nnnrss-read-links-function #'nnnrss--read-links
  nil nnfeed-read-links-function)

(defun nnnrss--read-parts (article)
  "Extract MIME PARTS."
  (if-let* ((content (dom-child-by-tag article 'description)))
      `((,(dom-text content) (Content-Type . "text/plain") links)
        (,(dom-text content) (Content-Type . "text/html") links))
    '((nil (Content-Type . "text/html") links))))
(defvoo nnnrss-read-parts-function #'nnnrss--read-parts
  nil nnfeed-read-parts-function)

(gnus-declare-backend (symbol-name nnnrss-backend) 'none 'address)

(provide 'nnnrss)
;;; nnnrss.el ends here
