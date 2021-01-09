;;; mu4e-marker-icons.el --- Display icons for mu4e markers. -*- lexical-binding: t; -*-
;; -*- coding: utf-8 -*-

;;; Time-stamp: <2021-01-09 19:43:20 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "26.1"))
;; Package-Version: 0.1.0
;; Keywords: email
;; homepage: https://github.com/stardiviner/mu4e-marker-icons

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.
;;
;; mu4e-marker-icons is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; mu4e-marker-icons is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage
;;
;; (mu4e-marker-icons-mode 1)

;;; Code:

(require 'mu4e-headers)

(defgroup mu4e-marker-icons nil
  "Display icons for mu4e markers."
  :group 'mu4e-marker-icons)

(defvar mu4e-marker-icons-marker-alist
  '((mu4e-headers-seen-mark . mu4e-marker-icons-saved-headers-seen-mark)
    (mu4e-headers-new-mark . mu4e-marker-icons-saved-headers-new-mark)
    (mu4e-headers-unread-mark . mu4e-marker-icons-saved-headers-unread-mark)
    (mu4e-headers-signed-mark . mu4e-marker-icons-saved-headers-signed-mark)
    (mu4e-headers-encrypted-mark . mu4e-marker-icons-saved-headers-encrypted-mark)
    (mu4e-headers-draft-mark . mu4e-marker-icons-saved-headers-draft-mark)
    (mu4e-headers-attach-mark . mu4e-marker-icons-saved-headers-attach-mark)
    (mu4e-headers-passed-mark . mu4e-marker-icons-saved-headers-passed-mark)
    (mu4e-headers-flagged-mark . mu4e-marker-icons-saved-headers-flagged-mark)
    (mu4e-headers-replied-mark . mu4e-marker-icons-saved-headers-replied-mark)
    (mu4e-headers-trashed-mark . mu4e-marker-icons-saved-headers-trashed-mark)
    ;; thread prefix marks
    (mu4e-headers-default-prefix . mu4e-marker-icons-saved-headers-default-prefix)
    (mu4e-headers-has-child-prefix . mu4e-marker-icons-saved-headers-has-child-prefix)
    (mu4e-headers-empty-parent-prefix . mu4e-marker-icons-saved-headers-empty-parent-prefix)
    (mu4e-headers-first-child-prefix . mu4e-marker-icons-saved-headers-first-child-prefix)
    (mu4e-headers-duplicate-prefix . mu4e-marker-icons-saved-headers-duplicate-prefix))
  "An alist of markers used in mu4e.")

(defun mu4e-marker-icons--store (l)
  (mapcar (lambda (x) (set (cdr x) (symbol-value (car x)))) l))

(defun mu4e-marker-icons--restore (l)
  (let ((lrev (mapcar (lambda (x) (cons (cdr x) (car x))) l)))
    (mu4e-save lrev)))


(defun mu4e-marker-icons-enable ()
  "Enable mu4e-marker-icons."
  (mu4e-marker-icons--store mu4e-marker-icons-marker-alist)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-precise-alignment t)
  (setq
   mu4e-headers-seen-mark `("S" . ,(propertize
                                    (all-the-icons-material "mail_outline")
                                    'face `(:family ,(all-the-icons-material-family)
                                                    :foreground ,(face-background 'default))))
   mu4e-headers-new-mark `("N" . ,(propertize
                                   (all-the-icons-material "markunread")
                                   'face `(:family ,(all-the-icons-material-family)
                                                   :foreground ,(face-background 'default))))
   mu4e-headers-unread-mark `("u" . ,(propertize
                                      (all-the-icons-material "notifications_none")
                                      'face 'mu4e-unread-face))
   mu4e-headers-signed-mark `("s" . ,(propertize
                                      (all-the-icons-material "check")
                                      'face `(:family ,(all-the-icons-material-family)
                                                      :foreground "DarkCyan")))
   mu4e-headers-encrypted-mark `("x" . ,(propertize
                                         (all-the-icons-material "enhanced_encryption")
                                         'face `(:family ,(all-the-icons-material-family)
                                                         :foreground "CornflowerBlue")))
   mu4e-headers-draft-mark `("D" . ,(propertize
                                     (all-the-icons-material "drafts")
                                     'face 'mu4e-draft-face))
   mu4e-headers-attach-mark `("a" . ,(propertize
                                      (all-the-icons-material "attachment")
                                      'face 'mu4e-attach-number-face))
   mu4e-headers-passed-mark `("P" . ,(propertize ; ❯ (I'm participated in thread)
                                      (all-the-icons-material "center_focus_weak")
                                      'face `(:family ,(all-the-icons-material-family)
                                                      :foreground "yellow")))
   mu4e-headers-flagged-mark `("F" . ,(propertize
                                       (all-the-icons-material "flag")
                                       'face 'mu4e-flagged-face))
   mu4e-headers-replied-mark `("R" . ,(propertize
                                       (all-the-icons-material "reply_all")
                                       'face 'mu4e-replied-face))
   mu4e-headers-trashed-mark `("T" . ,(propertize
                                       (all-the-icons-material "delete_forever")
                                       'face 'mu4e-trashed-face))
   ;; thread prefix marks
   mu4e-headers-default-prefix `("|" . ,(propertize
                                         (all-the-icons-material "message")
                                         'face `(:family ,(all-the-icons-material-family))))
   mu4e-headers-has-child-prefix `("+" . ,(propertize ; "Parent" ╰
                                           (all-the-icons-material "expand_more")
                                           'face `(:family ,(all-the-icons-material-family))))
   mu4e-headers-empty-parent-prefix `("-" . ,(propertize ; "Orphan"
                                              (all-the-icons-material "navigate_before")
                                              'face `(:family ,(all-the-icons-material-family))))
   mu4e-headers-first-child-prefix `("\\" . ,(propertize
                                              (all-the-icons-material "navigate_next")
                                              'face `(:family ,(all-the-icons-material-family))))
   mu4e-headers-duplicate-prefix `("=" . ,(propertize
                                           (all-the-icons-material "content_copy")
                                           'face `(:family ,(all-the-icons-material-family)
                                                           :foreground "siennan")))))

(defun mu4e-marker-icons-disable ()
  "Disable mu4e-marker-icons."
  (mu4e-marker-icons--restore mu4e-marker-icons-marker-alist))

;;;###autoload
(define-minor-mode mu4e-marker-icons-mode
  "Display icons for mu4e markers."
  :require 'mu4e-marker-icons-mode
  :init-value nil
  :global t
  (if mu4e-marker-icons-mode
      (mu4e-marker-icons-enable)
    (mu4e-marker-icons-disable)))



(provide 'mu4e-marker-icons)

;;; mu4e-marker-icons.el ends here
