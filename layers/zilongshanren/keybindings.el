;;; keybindings.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(bind-key* "C-c l" 'zilongshanren/insert-chrome-current-tab-url)
;;This command won't change the buffer, so it should be in normal-state
;; (bind-key* "C-c o" 'spacemacs/open-in-external-app)

(require 'dired)
(define-key dired-mode-map (kbd "<mouse-2>") 'my-dired-find-file)

(define-key dired-mode-map "r" 'dired-start-process)
(define-key dired-mode-map "E" 'dired-toggle-read-only)

(define-key dired-mode-map (kbd "`") 'dired-open-term)


(eval-after-load 'dired-mode
  (progn
    (define-key dired-mode-map (kbd "C-k") 'zilongshanren/dired-up-directory)))

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

(eval-after-load 'term '(define-key term-raw-map (kbd "C-:") 'dired-jump))

(define-key dired-mode-map (kbd "z") 'dired-get-size)
(define-key dired-mode-map (kbd "C-c C-e") 'dired-toggle-read-only)

(global-set-key (kbd "s-/") 'hippie-expand)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)


(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key [(shift return)] 'smart-open-line)

(define-key global-map (kbd "<f1>") 'zilongshanren/hotspots)
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)

(global-set-key (kbd "C-.") 'company-capf)


;; some easy functions for navigate functions
;;C-M-a beginning-of-defun
;;C-M-e end-of-defun
;;C-M-h mark-defun
(global-set-key (kbd "C-s-h") 'mark-defun)

(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "C-`") 'toggle-input-method)

(global-set-key (kbd "<f5>") 'zilongshanren/run-current-file)

;; "http://endlessparentheses.com/transposing-keybinds-in-emacs.html?source=rss"
;; (global-set-key "\C-t" #'transpose-lines)
;; (define-key ctl-x-map "\C-t" #'transpose-chars)

(when (spacemacs/system-is-mac)
 (evil-leader/set-key "o!" 'zilongshanren/iterm-shell-command))

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
    :status shadowsocks-proxy-mode
    :on (global-shadowsocks-proxy-mode)
    :off (global-shadowsocks-proxy-mode -1)
    :documentation "Toggle shadowsocks proxy mode."
    :evil-leader "toP")

(global-set-key (kbd "s-s") 'save-buffer)
(bind-key* "s-k" 'scroll-other-window-down)
(bind-key* "s-j"  'scroll-other-window)
(bind-key* "C-c /" 'company-files)
