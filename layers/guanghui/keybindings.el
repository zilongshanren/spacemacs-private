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

(global-set-key (kbd "s-/") 'hippie-expand)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)


(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key [(shift return)] 'zilongshanren/smart-open-line)

(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)

;; (global-set-key (kbd "C-.") 'company-capf)


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
 (spacemacs/set-leader-keys "o!" 'zilongshanren/iterm-shell-command))

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")

(global-set-key (kbd "s-s") 'save-buffer)
(bind-key* "s-k" 'scroll-other-window-down)
(bind-key* "s-j"  'scroll-other-window)
(bind-key* "C-c /" 'company-files)

(bind-key* "s-r" 'zilongshanren/browser-refresh--chrome-applescript)
(spacemacs/set-leader-keys "oac" 'zilongshanren/browser-refresh--chrome-applescript)

(bind-key* "s-;" 'zilongshanren/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'zilongshanren/delete-semicolon-at-the-end-of-this-line)

(bind-key* "s-," 'zilongshanren/insert-comma-at-the-end-of-this-line)
(bind-key* "C-s-," 'zilongshanren/delete-comma-at-the-end-of-this-line)

(bind-key* "C-=" 'er/expand-region)


(bind-key* "M--" 'zilongshanren/goto-match-paren)

(spacemacs/set-leader-keys "oll" 'zilongshanren/load-my-layout)
(spacemacs/set-leader-keys "ols" 'zilongshanren/save-my-layout)

(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "<f9>") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c i e") 'spacemacs/auto-yasnippet-expand)
;; http://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab
;; (define-key input-decode-map [?\C-i] [C-i])
;; (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)

(bind-key* "C-M-s-y" 'aya-expand)
(bind-key* "C-l" 'recenter)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(spacemacs/set-leader-keys "nl" 'spacemacs/evil-search-clear-highlight)

(bind-key* "C-c k" 'which-key-show-top-level)

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)
