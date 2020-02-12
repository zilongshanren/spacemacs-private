;;; config.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defmacro th/define-context-key (keymap key dispatch)
  "Define KEY in KEYMAP to execute according to DISPATCH.

        DISPATCH is a form that is evaluated and should return the
        command to be executed.

        If DISPATCH returns nil, then the command normally bound to KEY
        will be executed.

        Example:

          (th/define-context-key hs-minor-mode-map
             (kbd \"<C-tab>\")
             (cond
              ((not (hs-already-hidden-p))
               'hs-hide-block)
              ((hs-already-hidden-p)
               'hs-show-block)))

        This will make <C-tab> show a hidden block.  If the block is
        shown, then it'll be hidden."
  `(define-key ,keymap ,key
     `(menu-item "context-key" ignore
                 :filter ,(lambda (&optional ignored)
                            ,dispatch))))

(setq auto-coding-regexp-alist
      (delete (rassoc 'utf-16be-with-signature auto-coding-regexp-alist)
              (delete (rassoc 'utf-16le-with-signature auto-coding-regexp-alist)
                      (delete (rassoc 'utf-8-with-signature auto-coding-regexp-alist)
                              auto-coding-regexp-alist))))

(defun ffap-hexl-mode ()
  (interactive)
  (let ((ffap-file-finder 'hexl-find-file))
    (call-interactively 'ffap)))

(when (spacemacs/window-system-is-mac)
  (setq ns-pop-up-frames nil))

(global-prettify-symbols-mode 1)
(setq-default fill-column 80)

;; prevent dired window press o to split into three column
(setq-default split-width-threshold 200)

(setq recenter-positions '(top middle bottom))
;; delete the selection with a key press
(delete-selection-mode t)

;;add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; tramp, for sudo access
;; very slow!!!!
;; for profiling emacs --debug-init --timed-requires --profile
;; (require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
;; (setq tramp-default-method "ssh")

;; This line has very bad performance lose!!!!!!!!!!!!!!!!!!!
;; (set-default 'imenu-auto-rescan t)

;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
(remove-hook 'find-file-hooks 'vc-find-file-hook)
;; https://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; this settings will cause command `vc-annotate` failed.
;; 如果把 vc-handled-backends去掉，那么 vc-follow-symlinks 这个选项就会失效
;; 进而，如果你访问一个在版本控制里面的alias的话，它不会自动去访问原文件，这个是非常不爽的
;; (setq vc-handled-backends ())


(setq large-file-warning-threshold 100000000)
;;http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/

(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq url-show-status nil)



;;Don't ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; cleanup recent files
(defun zilongshanren/cleanup-recentf ()
  (progn
    (and (fboundp 'recentf-cleanup)
         (recentf-cleanup))))

(add-hook 'kill-emacs-hook #'zilongshanren/cleanup-recentf)

;; change evil initial mode state
(menu-bar-mode t)

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(defun spacemacs/check-large-file ()
  (when (> (buffer-size) 500000)
    (progn (fundamental-mode)
           (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      nil))

(add-hook 'find-file-hook 'spacemacs/check-large-file)

(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

(add-hook 'minibuffer-inactive-mode-hook
          #'(lambda() (set (make-local-variable 'semantic-mode) nil)
              (set (make-local-variable 'electric-pair-mode) nil)))

;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun zilongshanren/stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'zilongshanren/stop-using-minibuffer)

(setq tags-add-tables nil)

(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)

;; http://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(csetq ediff-diff-options "-w")

(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; (when (and (spacemacs/system-is-mswindows) window-system)
;;   (setq w32-pipe-read-delay 0.5))

;; FIXME: --vimgrep will break ivy-occur with wgrep
(setq counsel-async-split-string-re "\r?\n")
;; (setq counsel-ag-base-command  "ag --vimgrep --nocolor --nogroup %s")

(defvar spacemacs--counsel-commands
  '(;; --line-number forces line numbers (disabled by default on windows)
    ;; no --vimgrep because it adds column numbers that wgrep can't handle
    ;; see https://github.com/syl20bnr/spacemacs/pull/8065
    ("rg" . "rg  --smart-case --ignore-file '.rgignore' --no-heading --color never --line-number --max-columns 220 %s %S .")
    ("ag" . "ag --nocolor --nogroup %s %S .")
    ("pt" . "pt -e --nocolor --nogroup %s %S .")
    ("ack" . "ack --nocolor --nogroup %s %S .")
    ("grep" . "grep -nrP %s %S ."))
  "An alist of search commands and their corresponding commands
with options to run in the shell.")

;; search chinse must add this line
;; https://emacs-china.org/t/emacs-helm-ag/6764
(if (spacemacs/system-is-mswindows)
    (modify-coding-system-alist 'process "rg" '(utf-8 . chinese-gbk-dos))
  (modify-coding-system-alist 'process "rg" '(utf-8 . utf-8)))


;; https://emacs-china.org/t/advice/7566
(defun chunyang-advice-remove-button (function)
  "Add a button to remove advice."
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        ;; :around advice: ‘shell-command--shell-command-with-editor-mode’
        (while (re-search-forward "^:[-a-z]+ advice: [‘'`]\\(.+\\)[’'']$" nil t)
          (let ((advice (intern-soft (match-string 1))))
            (when (and advice (fboundp advice))
              (let ((inhibit-read-only t))
                (insert " » ")
                (insert-text-button
                 "Remove"
                 'action
                 ;; In case lexical-binding is off
                 `(lambda (_)
                    (message "Removing %s of advice from %s" ',function ',advice)
                    (advice-remove ',function #',advice)
                    (revert-buffer nil t))
                 'follow-link t)))))))))

(advice-add 'describe-function-1 :after #'chunyang-advice-remove-button)

(defun zilong-ag-edit (function)
  (when (get-buffer "*helm-ag-edit*")
    (kill-buffer "*helm-ag-edit*"))
  (if (not (= (count-windows) 2))
      (progn
        (split-window-right))))

;; (defun zilong-after-ag-edit (function)
;;   (ivy-occur-grep-mode))

(advice-add 'helm-ag--edit :before #'zilong-ag-edit)
;; (advice-add 'helm-ag--edit :after #'zilong-after-ag-edit)

(setq counsel-git-cmd "git ls-files -z --full-name -- ':!:*.js.meta' ':!:*.meta'")
