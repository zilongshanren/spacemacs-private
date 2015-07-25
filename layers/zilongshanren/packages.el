;;; packages.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq zilongshanren-packages
    '(
      ;; package names go here
      lispy
      lua-mode
      company
      discover-my-major
      ws-butler
      ;; rtags ;;very flow and difficult to configure
      cmake-font-lock
      ;; google-c-style
      cmake-mode
      company-c-headers
      flycheck
      helm-make
      ycmd
      markdown-mode
      org-octopress
      impatient-mode
      ;; moz-controller
      helm-github-stars
      elfeed
      swiper
      magit
      git-messenger
      helm-flyspell
      helm
      ace-window
      avy
      helm-ls-git
      mwe-log-commands
      keyfreq
      evil
      ox-reveal
      org-mac-link
      ;; worf
      org-download
      flycheck-package
      org
      deft
      nodejs-repl
      prodigy
      ))

;; List of packages to exclude.
(setq zilongshanren-excluded-packages '())

(defun zilongshanren/init-nodejs-repl ()
  (use-package nodejs-repl
    :defer t))

(defun zilongshanren/init-flycheck-package ()
  (use-package flycheck-package))

(defun zilongshanren/post-init-company-c-headers()
  (use-package company-c-headers
    :defer t
    :init(progn
           (setq company-c-headers-path-system
                 (quote
                  ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")))
           (setq company-c-headers-path-user
                 (quote
                  ("/Users/guanghui/cocos2d-x/cocos/platform" "/Users/guanghui/cocos2d-x/cocos" "." "/Users/guanghui/cocos2d-x/cocos/audio/include/")))
           )
    ))
(defun zilongshanren/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (evil-leader/set-key (kbd "mhm") 'discover-my-major)
      (evilify makey-key-mode makey-key-mode-get-key-map))))

(defun zilongshanren/init-lispy()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :config
    (progn
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))
    :init
    (progn
      (define-key evil-insert-state-map (kbd "C-y") 'lispy-yank)
      (define-key evil-insert-state-map (kbd "C-d") 'lispy-delete)
      (add-hook 'emacs-lisp-mode-hook (lambda ()(lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))))

(defun zilongshanren/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (progn
      (push 'company-dabbrev company-backends-lua-mode)
      (push 'company-etags company-backends-lua-mode)
      (add-hook 'lua-mode-hook 'evil-matchit-mode)
      (evil-leader/set-key-for-mode 'lua-mode
        "mhi" 'helm-imenu
        "mhd" 'helm-gtags-dwim
        "mhr" 'helm-gtags-find-rtag
        "mhs" 'helm-gtags-find-symbol
        "mhf" 'helm-gtags-find-files))))


(defun zilongshanren/post-init-company ()
    (setq company-minimum-prefix-length 1)
    (global-set-key (kbd "C-.") 'company-complete)
    (spacemacs|add-company-hook lua-mode)
    (spacemacs|add-company-hook nxml-mode)
    )


(defun zilongshanren/init-ws-butler ()
    (use-package ws-butler
      :diminish ws-butler-mode
      :init
      (progn
       (add-hook 'c-mode-common-hook 'ws-butler-mode)
       (add-hook 'python-mode-hook 'ws-butler-mode)
       (add-hook 'cython-mode-hook 'ws-butler-mode)
        )))

;; (defun zilongshanren/init-rtags ()
;;   (use-package rtags
;;     :init (require 'company-rtags)
;;     :config
;;     (progn
;;       (evil-leader/set-key-for-mode 'c++-mode
;;         "mtr" 'rtags-find-references
;;         "mts" 'rtags-find-symbol
;;         "mti" 'rtags-imenu
;;         "mtf" 'rtags-find-file
;;         "mtv" 'rtags-find-virtuals-at-point)
;;       )
;;     ))

(defun zilongshanren/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun zilongshanren/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun zilongshanren/post-init-cmake-mode ()
  (use-package cmake-mode
    :defer
    :config
    (progn
      (defun cmake-rename-buffer ()
        "Renames a CMakeLists.txt buffer to cmake-<directory name>."
        (interactive)
        (when (and (buffer-file-name)
                   (string-match "CMakeLists.txt" (buffer-name)))
          (setq parent-dir (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory (buffer-file-name)))))
          (setq new-buffer-name (concat "cmake-" parent-dir))
          (rename-buffer new-buffer-name t)))

      (add-hook 'cmake-mode-hook (function cmake-rename-buffer))
      )))


(defun zilongshanren/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config (progn
              (flycheck-package-setup)
              (setq flycheck-display-errors-function 'flycheck-display-error-messages)
              (setq flycheck-display-errors-delay 0.2))))

(defun zilongshanren/init-helm-make ()
  (use-package helm-make
    :defer t))

(defun zilongshanren/post-init-ycmd ()
  (setq ycmd-tag-files 'auto)
  (setq ycmd-request-message-level -1)
  (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/Github/ycmd/ycmd/__main__.py"))))

;; configs for writing
(defun zilongshanren/post-init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :config
    (progn
      (spacemacs|add-company-hook markdown-mode)
      (defun zilongshanren/markdown-to-html ()
        (interactive)
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name)

                       )
        (browse-url (format  "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

      (evil-leader/set-key-for-mode 'gfm-mode-map
        "mp" 'zilongshanren/markdown-to-html
        )
      (evil-leader/set-key-for-mode 'markdown-mode
        "mp" 'zilongshanren/markdown-to-html
        )
      )))

(defun zilongshanren/init-org-octopress ()
  (use-package org-octopress
    :init
    (progn
      (evilify org-octopress-summary-mode org-octopress-summary-mode-map)
      (add-hook 'org-octopress-summary-mode-hook
                #'(lambda () (local-set-key (kbd "q") 'bury-buffer)))
      (setq org-octopress-directory-top       "~/4gamers.cn/source")
      (setq org-octopress-directory-posts     "~/4gamers.cn/source/_posts")
      (setq org-octopress-directory-org-top   "~/4gamers.cn/source")
      (setq org-octopress-directory-org-posts "~/4gamers.cn/source/blog")
      (setq org-octopress-setup-file          "~/4gamers.cn/setupfile.org")
      )))

(defun zilongshanren/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn

      (defun zilongshanren-mode-hook()
        "my web mode hook for HTML REPL"
        (interactive)
        (impatient-mode)
        (httpd-start))

      (add-hook 'web-mode-hook 'zilongshanren-mode-hook)
      ))
  )

;; (defun zilongshanren/init-moz-controller ()
;;   (use-package moz-controller
;;     :init
;;     (moz-controller-global-mode t)
;;     :diminish moz-controller-mode))

(defun zilongshanren/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "andyque")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache")
      )))


(defun zilongshanren/init-elfeed ()
  (use-package elfeed
    :defer t
    :config
    (progn
      (global-set-key (kbd "C-x w") 'elfeed)

      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://sachachua.com/blog/feed/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://blog.codingnow.com/atom.xml"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://zh.lucida.me/atom.xml"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsnyc.org/atom.xml"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"
              ))

      (defun elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank)
      )))

(defun zilongshanren/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))

(defun zilongshanren/init-mwe-log-commands ()
  (use-package mwe-log-commands
    :init
    (progn
      (evil-leader/set-key
        "oll" 'mwe:log-keyboard-commands
        "olf" 'mwe:open-command-log-buffer)
      )
    ))

(defun zilongshanren/init-swiper ()
  "Initialize my package"
  (use-package swiper
    :init
    (progn
      ;; http://oremacs.com/2015/04/16/ivy-mode/
      ;; (ivy-mode -1)
      ;; (setq magit-completing-read-function 'ivy-completing-read)

      ;; http://oremacs.com/2015/04/19/git-grep-ivy/
      (defun counsel-git-grep-function (string &optional _pred &rest _u)
        "Grep in the current git repository for STRING."
        (split-string
         (shell-command-to-string
          (format
           "git --no-pager grep --full-name -n --no-color -i -e \"%s\""
           string))
         "\n"
         t))

      (defun counsel-git-grep ()
        "Grep for a string in the current git repository."
        (interactive)
        (let ((default-directory (locate-dominating-file
                                  default-directory ".git"))
              (val (ivy-read "pattern: " 'counsel-git-grep-function))
              lst)
          (when val
            (setq lst (split-string val ":"))
            (find-file (car lst))
            (goto-char (point-min))
            (forward-line (1- (string-to-number (cadr lst)))))))
      (use-package ivy
        :defer t
        :config
        (progn
          (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
          (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
          ))

      (define-key global-map (kbd "C-s") 'swiper)
      (setq ivy-use-virtual-buffers t)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "C-c j") 'counsel-git-grep))))


(defun zilongshanren/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (progn
      (define-key magit-log-mode-map (kbd "W") 'magit-copy-as-kill)
      (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
      (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
      (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
      (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes))
    :init
    (progn
      ;; Githu PR settings
      ;; "http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html"
      (setq magit-repository-directories '("~/cocos2d-x/"))

      (defun endless/visit-pull-request-url ()
        "Visit the current branch's PR on Github."
        (interactive)
        (browse-url
         (format "https://github.com/%s/pull/new/%s"
                 (replace-regexp-in-string
                  "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                  (magit-get "remote"
                             (magit-get-remote)
                             "url"))
                 (cdr (magit-get-remote-branch)))))


      (eval-after-load 'magit
        '(define-key magit-mode-map (kbd "s-g")
           #'endless/visit-pull-request-url))


      (defadvice magit-blame-mode (after magit-blame-change-to-emacs-state activate compile)
        "when entering magit blame mode, change evil normal state to emacs state"
        (if (evil-normal-state-p)
            (evil-emacs-state)
          (evil-normal-state))
        )

      (ad-activate 'magit-blame-mode)

      (defadvice git-timemachine-mode (after git-timemachine-change-to-emacs-state activate compile)
        "when entering git-timemachine mode, change evil normal state to emacs state"
        (if (evil-normal-state-p)
            (evil-emacs-state)
          (evil-normal-state)))

      (ad-activate 'git-timemachine-mode)

      (setq magit-process-popup-time 10)
      )))

(defun zilongshanren/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
     (defun my-vc-visit-file-revision (file rev)
       "Visit revision REV of FILE in another window.
With prefix argument, uses the current window instead.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
       ;; based on `vc-revision-other-window'.
       (interactive
        (let ((file (expand-file-name
                     (read-file-name
                      (if (buffer-file-name)
                          (format "File (%s): " (file-name-nondirectory
                                                 (buffer-file-name)))
                        "File: ")))))
          (require 'vc)
          (unless (vc-backend file)
            (error "File %s is not under version control" file))
          (list file (vc-read-revision
                      "Revision to visit (default is working revision): "
                      (list file)))))
       (require 'vc)
       (unless (vc-backend file)
         (error "File %s is not under version control" file))
       (let ((revision (if (string-equal rev "")
                           (vc-working-revision file)
                         rev))
             (visit (if current-prefix-arg
                        'switch-to-buffer
                      'switch-to-buffer-other-window)))
         (funcall visit (vc-find-revision file revision))))

     (define-key git-messenger-map (kbd "f") 'my-vc-visit-file-revision)
     )))

(defun zilongshanren/post-init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init
    (global-set-key (kbd "C-c s") 'helm-flyspell-correct)
    ))

(defun zilongshanren/post-init-helm ()
  (use-package helm
    :init
    (progn
      ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
      ;; discussion of these options.
      (setq helm-split-window-in-side-p t
            helm-move-to-line-cycle-in-source t
            helm-ff-search-library-in-sexp t
            helm-ff-file-name-history-use-recentf t)

      (setq helm-completing-read-handlers-alist
            '((describe-function . ido)
              (describe-variable . ido)
              (debug-on-entry . helm-completing-read-symbols)
              (find-function . helm-completing-read-symbols)
              (find-tag . helm-completing-read-with-cands-in-buffer)
              (ffap-alternate-file . nil)
              (tmm-menubar . nil)
              (dired-do-copy . nil)
              (dired-do-rename . nil)
              (dired-create-directory . nil)
              (find-file . ido)
              (copy-file-and-rename-buffer . nil)
              (rename-file-and-buffer . nil)
              (w3m-goto-url . nil)
              (ido-find-file . nil)
              (ido-edit-input . nil)
              (mml-attach-file . ido)
              (read-file-name . nil)
              (yas/compile-directory . ido)
              (execute-extended-command . ido)
              (minibuffer-completion-help . nil)
              (minibuffer-complete . nil)
              (c-set-offset . nil)
              (wg-load . ido)
              (rgrep . nil)
              (read-directory-name . ido)
              )))))



(defun zilongshanren/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (progn
      (setq avi-keys
            '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u))
      (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
      (global-set-key (kbd "C-x C-o") #'ace-window))))

(defun zilongshanren/init-avy ()
  (use-package avy
    :defer t
    :init
    (progn
      (require 'ace-pinyin)
      (setq ace-pinyin-use-avy t)
      (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
      (global-set-key (kbd "M-s") 'avy-goto-char-2)
      (evil-leader/set-key "SPC" 'avy-goto-char-2)
      (global-set-key (kbd "C-c SPC") 'avy-goto-char-2)
      (evil-leader/set-key "l" 'avy-goto-line)
      )))

(defun zilongshanren/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :init
    (evil-leader/set-key "pf" 'helm-ls-git-ls)
    :config
    (setq helm-ls-git-show-abs-or-relative 'relative)))


;;configs for EVIL mode
(defun zilongshanren/post-init-evil ()
  (use-package evil
    :defer t
    :config
    (progn
      ;;mimic "nzz" behaviou in vim
      (defadvice evil-ex-search-next (after advice-for-evil-search-next activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      (defadvice evil-ex-search-previous (after advice-for-evil-search-previous activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

      (define-key evil-normal-state-map
        (kbd "Y") 'zilongshanren/yank-to-end-of-line)

      ;; rebind g,k to gj and gk
      (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

      (define-key evil-insert-state-map "\C-e" 'end-of-line)
      (define-key evil-insert-state-map "\C-n" 'next-line)
      (define-key evil-insert-state-map "\C-p" 'previous-line)
      (define-key evil-insert-state-map "\C-k" 'kill-line)
      (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
      (define-key evil-insert-state-map (kbd "s-b") 'backward-word)

      (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
      (define-key evil-ex-completion-map "\C-b" 'backward-char)
      (define-key evil-ex-completion-map "\C-k" 'kill-line)
      (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

      (define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
      (define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
      (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
      (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
      (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
      (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)

      ;; in spacemacs, we always use evilify miscro state
      (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

      (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
      (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
      (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
      (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

      ;; for emacs shell mode
      (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
      (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
      (evil-define-key 'emacs term-raw-map (kbd "C-w")
        'evil-delete-backward-word)
      (define-key evil-emacs-state-map (kbd "s-p") 'projectile-switch-project)

      (evil-leader/set-key "fR" 'rename-file-and-buffer)
      )))

(defun zilongshanren/init-org-mac-link ()
  (use-package org-mac-link
    :init
    (add-hook 'org-mode-hook (lambda ()
                               (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))))


(defun zilongshanren/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (progn
      (setq org-reveal-root "file:///Users/guanghui/.emacs.d/reveal-js")
      )))

;; (defun zilongshanren/init-worf ()
;;   (use-package worf
;;     :defer t
;;     :init
;;     (add-hook 'org-mode-hook 'worf-mode)))

(defun zilongshanren/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (org-download-enable)))

;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun zilongshanren/post-init-org ()
  (progn
    (spacemacs|add-company-hook org-mode)
    (evil-leader/set-key-for-mode 'org-mode
      "." 'org-agenda
      "mls" 'org-store-link
      "mli" 'org-insert-link)
    (require 'ox-publish)
    (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (setq org-latex-default-class "ctexart")
    (setq org-latex-pdf-process
          '(
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "rm -fr %b.out %b.log %b.tex auto"))

    (setq org-latex-listings t)
    ;; improve org babel

    (org-babel-do-load-languages
     'org-babel-load-languages
     '( (perl . t)
        (ruby . t)
        (sh . t)
        (python . t)
        (emacs-lisp . t)
        (C . t)
        (R . t)
        (ditaa . t)))

    (evil-leader/set-key-for-mode 'org-mode
      "mBc" 'org-babel-remove-result)
    ;; config for org export
    (require 'ox-publish)
    (defvar zilongshanren-website-html-preamble
      "<div class='nav'>
<ul>
<li><a href='http://zilongshanren.com'>博客</a></li>
<li><a href='/index.html'>笔记目录</a></li>
</ul>
</div>")
    (defvar zilongshanren-website-html-blog-head
      " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
    (setq org-publish-project-alist
          `(
            ("blog-notes"
             :base-directory "~/org-notes/wiki"
             :base-extension "org"
             :publishing-directory "~/org-notes/public_html/"

             :recursive t
             :html-head , zilongshanren-website-html-blog-head
             :publishing-function org-html-publish-to-html
             :headline-levels 4           ; Just the default for this project.
             :auto-preamble t
             :section-numbers nil
             :html-preamble ,zilongshanren-website-html-preamble
             :author "zilongshanren"
             :email "guanghui8827@gmail.com"
             :auto-sitemap t                ; Generate sitemap.org automagically...
             :sitemap-filename "sitemap.org" ; ... call it sitemap.org (it's the default)...
             :sitemap-title "Sitemap"        ; ... with title 'Sitemap'.
             :sitemap-sort-files anti-chronologically
             :sitemap-file-entry-format "%d %t"
             )
            ("blog-static"
             :base-directory "~/org-notes/wiki"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory "~/org-notes/public_html/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("blog" :components ("blog-notes" "blog-static"))
            ))
    (setq org-agenda-custom-commands
          '(("O" tags-todo "WORK")
            ("P" tags-todo "PROJECT")))


    ))

(defun zilongshanren/post-init-deft ()
  (setq deft-use-filter-string-for-filename t)
  (evil-leader/set-key-for-mode 'deft-mode "mq" 'quit-window)
  (setq deft-extension "org")
  (setq deft-directory "~/org-notes/wiki"))



(defun zilongshanren/post-init-prodigy ()
  (prodigy-define-tag
    :name 'jekyll
    :env '(("LANG" "en_US.UTF-8")
           ("LC_ALL" "en_US.UTF-8")))
  ;; define service
  (prodigy-define-service
    :name "Python app"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6001")
    :cwd "~/cocos2d-x/web"
    :tags '(work)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Octopress preview"
    :command "rake"
    :args '("preview")
    :cwd "~/4gamers.cn"
    :tags '(octopress jekyll)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t))
