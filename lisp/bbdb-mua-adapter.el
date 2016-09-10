;;; bbdb-mua-adapter.el --- BBDB mail user agent integration

;; Copyright (C) 2016  David Maus

;; Author: David Maus <dmaus@dmaus.name>
;; Keywords: mail

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

;; 

;;; Code:

(require 'eieio)
(require 'bbdb-mua)

(defvar bbdb-mua-adapter-instance nil
  "Current mail user agent.")

(defclass bbdb-mua-adapter () ()
  "Base class of mail user agent adapters.")

(defgeneric bbdb-mua-adapter-message-header (mua header)
  )

(defgeneric bbdb-mua-adapter-message-body (mua)
  )

(defgeneric bbdb-mua-adapter-update-records (mua &optional header-class update-p sort)
  )

(defgeneric bbdb-mua-adapter-get-address-components (mua &optional header-class ignore-address)
  )

(defmethod bbdb-mua-adapter-message-body ((mua bbdb-mua-adapter))
  (current-buffer))

(defmethod bbdb-mua-adapter-update-records ((mua bbdb-mua-adapter) &optional header-class update-p sort)
  (bbdb-update-records (bbdb-mua-adapter-get-address-components mua header-class) update-p sort))

(defmethod bbdb-mua-adapter-get-address-components ((mua bbdb-mua-adapter) &optional header-class ignore-address)
  ;; We do not use `bbdb-message-all-addresses' here because only when we
  ;; have compared the addresses with the records in BBDB do we know which
  ;; address(es) are relevant for us.
  (let ((message-headers (if header-class
                             (list (assoc header-class bbdb-message-headers))
                           bbdb-message-headers))
        (ignore-address (or ignore-address bbdb-user-mail-address-re))
        address-list address name mail mail-list content)
    (dolist (headers message-headers)
      (dolist (header (cdr headers))
        (when (setq content (bbdb-mua-adapter-message-header mua header))
          ;; Always extract all addresses because we do not know yet which
          ;; address might match IGNORE-ADDRESS.
          (dolist (address (bbdb-extract-address-components content t))
            ;; We canonicalize name and mail as early as possible.
            (setq name (car address)
                  mail (cadr address))
            ;; ignore uninteresting addresses
            (unless (or (and (stringp ignore-address)
                             (or (and name (string-match ignore-address name))
                                 (and mail (string-match ignore-address mail))))
                        (and mail (member-ignore-case mail mail-list)))
              ;; Add each address only once. (Use MAIL-LIST for book keeping.)
              ;; Thus if we care about whether an address gets associated with
              ;; one or another header, the order of elements in
              ;; `bbdb-message-headers' is relevant.  The "most important"
              ;; headers should be first in `bbdb-message-headers'.
              (if mail (push mail mail-list))
              ;; dmaus, 2016: Removed symbol indicating mua from the list
              (push (list name mail header (car headers)) address-list))))))
    (or (nreverse address-list)
        (and header-class bbdb-message-try-all-headers
             ;; Try again the remaining header classes
             (let ((bbdb-message-headers
                    (remove (assoc header-class bbdb-message-headers)
                            bbdb-message-headers)))
               (bbdb-mua-adapter-get-address-components mua nil ignore-address))))))

;; Moved from bbdb-mua

(defun bbdb-get-address-components (&optional header-class ignore-address)
  "Extract mail addresses from a message.
Return list with elements (NAME EMAIL HEADER HEADER-CLASS MUA).
HEADER-CLASS is defined in `bbdb-message-headers'.  If HEADER-CLASS is nil,
use all classes in `bbdb-message-headers'.
If regexp IGNORE-ADDRESS matches NAME or EMAIL of an address, this address
is ignored. If IGNORE-ADDRESS is nil, use value of `bbdb-user-mail-address-re'."
  (bbdb-mua-adapter-get-address-components bbdb-mua-adapter-instance header-class ignore-address))

;;;###autoload
(defun bbdb-message-header (header)
  "For the current message return the value of HEADER.
MIME encoded headers are decoded.  Return nil if HEADER does not exist."
  ;; RW: If HEADER was allowed to be a regexp and the content of multiple
  ;; matching headers was concatenated as in `message-field-value',
  ;; this would simplify the usage of `bbdb-accept-message-alist' and
  ;; `bbdb-ignore-message-alist'.
  ;; RW: If this function had a remember table, it could look up the value
  ;; of a header if we request the value of the same header multiple times.
  ;; (We would reset the remember table each time we move on to a new message.)
  (let ((val (bbdb-mua-adapter-message-header bbdb-mua-adapter-instance header)))
    (if val (mail-decode-encoded-word-string val))))

(defun bbdb-mua-update-records (&optional header-class update-p sort)
  "Wrapper for `bbdb-update-records'.
HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.
UPDATE-P is defined in `bbdb-update-records'.
If SORT is non-nil, sort records according to `bbdb-record-lessp'."
  (bbdb-mua-adapter-update-records bbdb-mua-adapter-instance header-class update-p sort))

(defmacro bbdb-mua-wrapper (&rest body)
  (declare (debug t))
  `(with-current-buffer (bbdb-mua-adapter-message-body bbdb-mua-adapter-instance)
    ,@body))

(provide 'bbdb-mua-adapter)

;;; bbdb-mua-adapter.el ends here
