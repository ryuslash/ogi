;;; ogi.el --- Github issues in org-mode

;; Copyright (C) 2012  Tom Willemsen

;; Author: Tom Willemsen <tom@ryuslash.org>
;; Keywords: convenience, extensions
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Import github issues into org-mode.

(require 'json)
(require 'org)

;;; Code:

(defun ogi-get (project)
  "Get a list of open issues for PROJECT."
  (let* ((url-request-extra-headers
         `(("Authorization" . ,(concat
                                "Basic "
                                (base64-encode-string
                                 (concat (read-string "Username: ")
                                         ":"
                                         (read-passwd "Password: ")))))))
        (buffer (url-retrieve-synchronously
                 (concat "https://api.github.com/repos/" project
                         "/issues?state=open")))
        result)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq result (json-read)))
    (kill-buffer buffer)
    result))

(defmacro ogiprop (obj prop)
  "From alist OBJ get the value of PROP."
  `(cdr (assq ',prop ,obj)))

;;;###autoload
(defun ogi-insert (project)
  "Insert (new) issues for PROJECT under the current entry."
  (interactive "MGet issues for:")
  (goto-char (point-max))
  (mapc (lambda (issue)
          (let ((id (number-to-string (ogiprop issue id))))
            (unless (org-find-entry-with-id id)
              (org-insert-heading-after-current)
              (insert (ogiprop issue title))
              (newline)
              (insert (ogiprop issue body))
              (fill-paragraph)
              (org-todo (if (string= (ogiprop issue state) "closed")
                            'done
                          "TODO"))
              (org-set-tags-to (mapcar (lambda (itm)
                                         (replace-regexp-in-string
                                          "-" "_" (ogiprop itm name)))
                                       (ogiprop issue labels)))
              (org-entry-put (point) "ID" id))))
        (ogi-get project)))

(provide 'ogi)

;;; ogi.el ends here
