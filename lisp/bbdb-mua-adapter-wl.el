;;; bbdb-mua-adapter-wl.el --- BBDB Wanderlust user agent adapter

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

(require 'bbdb-mua-adapter)

(defclass bbdb-mua-adapter:wanderlust (bbdb-mua-adapter) ()
  "BBDB Wanderlust adapter.")

(defmethod bbdb-mua-adapter-message-header ((mua bbdb-mua-adapter:wanderlust) header)
  (elmo-message-entity-field
   (elmo-message-entity wl-summary-buffer-elmo-folder
                        (wl-summary-message-number))
   (intern (downcase header)) 'string))

;; Moved from bbdb-wl

;;;###autoload
(defun bbdb-insinuate-wl ()
  "Hook BBDB into Wanderlust."
  (setq bbdb-mua-adapter-instance (make-instance 'bbdb-mua-adapter:wanderlust))
  (define-key wl-summary-mode-map (kbd ":") #'bbdb-mua-display-sender)
  (define-key wl-summary-mode-map (kbd ";") #'bbdb-mua-edit-field-sender)
  (when bbdb-complete-mail
    (define-key wl-draft-mode-map (kbd "M-;") #'bbdb-complete-mail)
    (define-key wl-draft-mode-map (kbd "M-<tab>") #'bbdb-complete-mail)))

(provide 'bbdb-mua-adapter-wl)

;;; bbdb-mua-adapter-wl.el ends here
