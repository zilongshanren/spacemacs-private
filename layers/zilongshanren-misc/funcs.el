;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; force update evil keymaps after git-timemachine-mode loaded
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))


;; insert ; at the end of current line
(defun zilongshanren/insert-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun zilongshanren/delete-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ";")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun zilongshanren/insert-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defun zilongshanren/delete-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ",")
        (progn
          (backward-char)
          (delete-char 1)))))


(defun zilongshanren/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "zilong")))

(defun zilongshanren/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "zilong")))

;; http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
;; ;FIXME: make it work with zsh
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Bash history:") collection))))
      (kill-new val)
      (message "%s => kill-ring" val))))

  ;; my fix for tab indent
(defun zilongshanren/indent-region(numSpaces)
  (progn
                                      ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

                                      ; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion                          ; restore the position afterwards
      (goto-char regionStart)                ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd)                  ; go to the end of region
      (setq end (line-end-position))         ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil)           ; restore the selected region
      )
    )
  )


(defun zilongshanren/tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (zilongshanren/indent-region 4)               ; region was selected, call indent-region
    (insert "    ")                   ; else insert four spaces as expected
    ))

(defun zilongshanren/untab-region (N)
  (interactive "p")
  (zilongshanren/indent-region -4))

(defun zilongshanren/hack-tab-key ()
  (interactive)
  (local-set-key (kbd "<tab>") 'zilongshanren/tab-region)
  (local-set-key (kbd "<S-tab>") 'zilongshanren/untab-region)
  )

;; I'm don't like this settings too much.
;; (add-hook 'prog-mode-hook 'zilongshanren/hack-tab-key)
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun my-unwind-git-timemachine ()
  (if (not (eq last-command-event 13))
      (git-timemachine-quit)))

;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :unwind #'my-unwind-git-timemachine
              :action (lambda (rev)
                        (git-timemachine-show-revision (cdr rev))))))

(defun my-git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))


(defun zilongshanren/helm-hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(zilongshanren//hotspots-sources))))

(defun zilongshanren//hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "Mail and News")
    (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                   ("RSS" . elfeed)
                   ("Blog" . blog-admin-start)
                   ("Github" . (lambda() (helm-github-stars)))
                   ("Calculator" . (lambda () (helm-calcul-expression)))
                   ("Run current flie" . (lambda () (zilongshanren/run-current-file)))
                   ("Agenda" . (lambda () (org-agenda "" "a")))
                   ("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))

;; insert date and time
(defun zilongshanren/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun zilongshanren/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; https://github.com/syohex/emacs-browser-refresh/blob/master/browser-refresh.el
(defun zilongshanren/browser-refresh--chrome-applescript ()
  (interactive)
  (do-applescript
   (format
    "
  tell application \"Google Chrome\"
    set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
    set winref's index to 1
    reload active tab of winref
  end tell
" )))


(define-minor-mode
  shadowsocks-proxy-mode
  :global t
  :init-value nil
  :lighter " SS"
  (if shadowsocks-proxy-mode
      (setq url-gateway-method 'socks)
    (setq url-gateway-method 'native)))


(define-global-minor-mode
  global-shadowsocks-proxy-mode shadowsocks-proxy-mode shadowsocks-proxy-mode
  :group 'shadowsocks-proxy)


(defun zilongshanren/open-file-with-projectile-or-counsel-git ()
  (interactive)
  (if (zilongshanren/git-project-root)
      (counsel-git)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump))))


;; http://blog.lojic.com/2009/08/06/send-growl-notifications-from-carbon-emacs-on-osx/
(defun zilongshanren/growl-notification (title message &optional sticky)
  "Send a Growl notification"
  (do-applescript
   (format "tell application \"GrowlHelperApp\" \n
              notify with name \"Emacs Notification\" title \"%s\" description \"%s\" application name \"Emacs.app\" sticky \"%s\"
              end tell
              "
           title
           message
           (if sticky "yes" "no"))))

(defun zilongshanren/growl-timer (minutes message)
  "Issue a Growl notification after specified minutes"
  (interactive (list (read-from-minibuffer "Minutes: " "10")
                     (read-from-minibuffer "Message: " "Reminder") ))
  (run-at-time (* (string-to-number minutes) 60)
               nil
               (lambda (minute message)
                 (zilongshanren/growl-notification "Emacs Reminder" message t))
               minutes
               message))

(defun zilongshanren/goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

(defun zilongshanren/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun zilongshanren/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun zilongshanren/insert-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (zilongshanren/retrieve-chrome-current-tab-url)))

(defun zilongshanren/retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  "	set theUrl to get URL of active tab of first window\n"
                  "	set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))


;; remove all the duplicated emplies in current buffer
(defun zilongshanren/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;; for running long run ansi-term
(defun zilongshanren/named-term (name)
  (interactive "sName: ")
  (ansi-term "/bin/zsh" name))


(defun zilongshanren/ash-term-hooks ()
  ;; dabbrev-expand in term
  (define-key term-raw-escape-map "/"
    (lambda ()
      (interactive)
      (let ((beg (point)))
        (dabbrev-expand nil)
        (kill-region beg (point)))
      (term-send-raw-string (substring-no-properties (current-kill 0)))))
  ;; yank in term (bound to C-c C-y)
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (current-kill 0)))))

(defun zilongshanren/terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer-other-window "*ansi-term*")
    (progn
      (split-window-right-and-focus)
      (ansi-term "/bin/zsh")))
  (get-buffer-process "*ansi-term*"))

(defalias 'tt 'zilongshanren/terminal)

;;add count for chinese, mainly used for writing chinese blog post
;; http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun zilongshanren/word-count-for-chinese ()
  "「較精確地」統計中/日/英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。

※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以#+開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                        ; 把註解行刪掉（不把註解算進字數）。
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字數（不含標點）：%s
中日文字數（包含標點）：%s
英文字數（不含標點）：%s
=======================
中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))

(defun zilongshanren/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

(defun zilongshanren/git-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))


;; "http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/"
(defun zilongshanren/iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (zilongshanren/git-project-root)
                default-directory))
         ;; if COMMAND is empty, just change directory
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      "
  tell application \"iTerm2\"
       activate
       set _session to current session of current window
       tell _session
            set command to get the clipboard
            write text \"%s\"
       end tell
  end tell
  " cmd))))


(defadvice persp-switch (after my-quit-helm-perspectives activate)
  (setq hydra-deactivate t))

(defun zilongshanren/my-mc-mark-next-like-this ()
  (interactive)
  (if (region-active-p)
      (mc/mark-next-like-this 1)
    (er/expand-region 1)))


(defun wrap-sexp-with-new-round-parens ()
  (interactive)
  (insert "()")
  (backward-char)
  (sp-forward-slurp-sexp))

(defun evil-paste-after-from-0 ()
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (zilongshanren/growl-notification
     (concat "ERC: : " (buffer-name (current-buffer)))
     message
     t
     )))

(defun my-swiper-search (p)
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'spacemacs/swiper-region-or-symbol
       #'counsel-grep-or-swiper))))

(defun ivy-ff-checksum ()
  (interactive)
  "Calculate the checksum of FILE. The checksum is copied to kill-ring."
  (let ((file (expand-file-name (ivy-state-current ivy-last) ivy--directory))
        (algo (intern (ivy-read
                       "Algorithm: "
                       '(md5 sha1 sha224 sha256 sha384 sha512)))))
    (kill-new (with-temp-buffer
                (insert-file-contents-literally file)
                (secure-hash algo (current-buffer))))
    (message "Checksum copied to kill-ring.")))

(defun ivy-ff-checksum-action (x)
  (ivy-ff-checksum))

(defun my-find-file-in-git-repo (repo)
  (if (file-directory-p repo)
      (let* ((default-directory repo)
             (files (split-string (shell-command-to-string (format "cd %s && git ls-files" repo)) "\n" t)))
        (ivy-read "files:" files
                  :action 'find-file
                  :caller 'my-find-file-in-git-repo))
    (message "%s is not a valid directory." repo)))

(defun my-open-file-in-external-app (file)
  "Open file in external application."
  (interactive)
  (let ((default-directory (zilongshanren/git-project-root))
        (file-path file))
    (if file-path
        (cond
         ((spacemacs/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
         ((spacemacs/system-is-linux) (let ((process-connection-type nil))
                                        (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun ivy-insert-action (x)
  (with-ivy-window
    (insert x)))

(defun ivy-kill-new-action (x)
  (with-ivy-window
    (kill-new x)))

(defun counsel-goto-recent-directory ()
  "Recent directories"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :action 'dired
              :caller 'counsel-goto-recent-directory)))

(defun counsel-find-file-recent-directory ()
  "Find file in recent git repository."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :action 'my-find-file-in-git-repo
              :caller 'counsel-find-file-recent-directory)))

(defun zilongshanren/magit-visit-pull-request ()
  "Visit the current branch's PR on GitHub."
  (interactive)
  (let ((remote-branch (magit-get-current-branch)))
    (cond
     ((null remote-branch)
      (message "No remote branch"))
     (t
      (browse-url
       (format "https://github.com/%s/pull/new/%s"
               (replace-regexp-in-string
                "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                (magit-get "remote"
                           (magit-get-remote)
                           "url"))
               remote-branch))))))

(defun zilongshanren/markdown-to-html ()
  (interactive)
  (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
  (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

(defun github-browse-file--relative-url ()
  "Return \"username/repo\" for current repository.

Error out if this isn't a GitHub repo."
  (require 'vc-git)
  (let ((url (vc-git--run-command-string nil "config" "remote.origin.url")))
    (unless url (error "Not in a GitHub repo"))
    (when (and url (string-match "github.com:?/?\\(.*\\)" url))
      (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))

(defun zilong/github-browse-commit ()
  "Show the GitHub page for the current commit."
  (interactive)
  (let* ((commit git-messenger:last-commit-id)
         (url (concat "https://github.com/"
                      (github-browse-file--relative-url)
                      "/commit/"
                      commit)))
    (browse-url url)
    (git-messenger:popup-close)))

(defun zilongshanren/search-in-fireball ()
  (interactive)
  (helm-do-ag (expand-file-name "~/Github/fireball/")))


(defun zilongshanren/show-current-buffer-major-mode ()
  (interactive)
  (describe-variable 'major-mode))

(defun zilongshanren/counsel-imenu ()
  (interactive)
  (counsel-imenu)
  (evil-set-jump))
