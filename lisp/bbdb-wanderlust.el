;;; bbdb-wanderlust.el --- BBDB interface to Wanderlust

;; Copyright (C) 2014 David Maus <dmaus@ictsoc.de>

;; This file is NOT part of the Insidious Big Brother Database (aka
;; BBDB).

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'bbdb-mua)

(defun bbdb/wanderlust-header (header)
  (elmo-message-entity-field
   (elmo-message-entity wl-summary-buffer-elmo-folder (wl-summary-message-number))
   (intern (downcase header)) 'string))

;;;###autoload
(defun bbdb-insinuate-wanderlust ()
  "Hook BBDB into Wanderlust."
  (define-key wl-summary-mode-map (kbd ":") #'bbdb-mua-display-sender)
  (define-key wl-summary-mode-map (kbd ";") #'bbdb-mua-edit-field-sender)
  (when bbdb-complete-mail
    (define-key wl-draft-mode-map (kbd "M-;") #'bbdb-complete-mail)
    (define-key wl-draft-mode-map (kbd "M-<tab>") #'bbdb-complete-mail)))

(add-to-list 'bbdb-mua-mode-alist '(wanderlust wl-summary-mode wl-draft-mode))
(add-to-list 'bbdb-init-forms '(wanderlust (add-hook 'wl-init-hook #'bbdb-insinuate-wanderlust)))

(defadvice bbdb-message-header (around bbdb-message-header/wanderlust activate)
  (if (eq 'wanderlust (bbdb-mua))
      (setq ad-return-value (bbdb/wanderlust-header (ad-get-arg 0)))
    ad-do-it))

(defadvice bbdb-mua-update-records (around bbdb-mua-update-records/wanderlust activate)
  (if (eq 'wanderlust (bbdb-mua))
      (setq ad-return-value (bbdb-update-records (bbdb-get-address-components (ad-get-arg 0))
                                                 (ad-get-arg 1) (ad-get-arg 2)))
    ad-do-it))

(provide 'bbdb-wanderlust)

;;; bbdb-wanderlust.el ends here
