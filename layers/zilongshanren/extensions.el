;;; extensions.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq zilongshanren-post-extensions
      '(
        ;; post extension names go here
        ;; doxymacs
        ;; nodejs-repl-eval don't support es6 and js2-mode also don't support it
        ;; so I use js-comit instead.
        ;; nodejs-repl-eval
        ;; plain-org-wiki
        ))

;; For each extension, define a function zilongshanren/init-<extension-name>
;;
(defun zilongshanren/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    :config
    (progn
      (defun my-doxymacs-font-lock-hook ()
        (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
            (doxymacs-font-lock)))
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (spacemacs|hide-lighter doxymacs-mode)))) 
;; https://atlanis.net/blog/posts/nodejs-repl-eval.html
(defun zilongshanren/init-nodejs-repl-eval ()
  (use-package nodejs-repl-eval
    :init))

(defun zilongshanren/init-plain-org-wiki ()
  (use-package plain-org-wiki
    :init
    (setq pow-directory "~/org-notes")))
