;;; plain-org-wiki.el --- Simple jump-to-org-files in a directory package

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Version: 0.1.0
;; Keywords: completion

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'ivy)

(defgroup plain-org-wiki nil
  "Simple jump-to-org-file package."
  :group 'org
  :prefix "plain-org-wiki-")

(defcustom pow-directory "~/org/wiki/"
  "Directory where files for `plain-org-wiki' are stored."
  :type 'directory)

(defun pow-files ()
  "Return .org files in `pow-directory'."
  (let ((default-directory pow-directory))
    (mapcar #'file-name-sans-extension
            (file-expand-wildcards "*.org"))))

(defun pow-files-recursive ()
  "Return .org files in `pow-directory' and subdirectories."
  (let ((ffip-project-root pow-directory))
    (delq nil
          (mapcar (lambda (x)
                    (when (equal (file-name-extension (car x)) "org")
                      (file-name-sans-extension (car x))))
                  (ffip-project-files)))))

(defun pow-find-file (x)
  "Open X as a file with org extension in `pow-directory'."
  (find-file (expand-file-name
              (format "%s.org" x)
              pow-directory)))

;;;###autoload
(defun plain-org-wiki-helm ()
  "Select an org-file to jump to."
  (interactive)
  (require 'helm)
  (require 'helm-match-plugin)
  (helm :sources
        '(((name . "Projects")
           (candidates . pow-files)
           (action . pow-find-file))
          ((name . "Create org-wiki")
           (dummy)
           (action . pow-find-file)))))

;;;###autoload
(defun plain-org-wiki ()
  "Select an org-file to jump to."
  (interactive)
  (let ((r (ivy-read "pattern: " (pow-files))))
    (when r
      (pow-find-file r))))

(provide 'plain-org-wiki)

;;; plain-org-wiki.el ends here
