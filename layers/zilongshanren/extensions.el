;;; extensions.el --- zilongshanren Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq zilongshanren-post-extensions
      '(
        ;; post extension names go here
        doxymacs
        nodejs-repl-eval
        ))

(setq zilongshanren-pre-extensions
      '())

;; For each extension, define a function zilongshanren/init-<extension-name>
;;
(defun zilongshanren/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (progn
      ;; (defun my-doxymacs-font-lock-hook ()
      ;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      ;;       (doxymacs-font-lock)))
      ;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (add-hook 'c-mode-common-hook 'doxymacs-mode)
      (spacemacs|hide-lighter doxymacs-mode)
      ))
  )

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
