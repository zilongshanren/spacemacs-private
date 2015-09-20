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
        doxymacs
        nodejs-repl-eval
        plain-org-wiki
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
    :init
    (use-package js2-mode
      :defer t
      :config
      (define-key js2-mode-map (kbd "C-x C-e") 'nodejs-repl-eval-dwim)
      (evil-leader/set-key-for-mode 'js2-mode
        "msd" 'nodejs-repl-eval-dwim)
      )))

(defun zilongshanren/init-plain-org-wiki ()
  (use-package plain-org-wiki
    :init
    (setq pow-directory "~/org-notes")))
