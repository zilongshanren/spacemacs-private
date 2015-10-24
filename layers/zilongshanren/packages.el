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
        css-mode
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
        ycmd ;;It's very slow and company-gtags is enough
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
        org-pomodoro
        ;; worf
        org-download
        flycheck-package
        org
        deft
        nodejs-repl
        prodigy
        js2-mode
        visual-regexp
        visual-regexp-steroids
        popwin
        helm-gtags
        multiple-cursors
        hydra
        persp-mode
        json-mode
        chinese-wbim
        racket-mode
        yasnippet
        youdao-dictionary
        powerline
        helm-ag
        cc-mode
        whitespace
        hungry-delete
        evil-escape
        org-bullets
        ;; flyspell
        find-file-in-project
        hl-anything
        projectile
        wrap-region
        org-tree-slide
        vinegar
        web-mode 
        ;; tagedit
        ))

(defun zilongshanren/post-init-web-mode ()
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))

(defun zilongshanren/post-init-vinegar ()
  (when (configuration-layer/layer-usedp 'vinegar)
    (evilify dired-mode dired-mode-map
             (kbd "C-k") 'zilongshanren/dired-up-directory
             (kbd "C") 'dired-do-copy)))

(defun zilongshanren/init-org-tree-slide ()
  (use-package org-tree-slide
    :init
    (evil-leader/set-key "oto" 'org-tree-slide-mode)))

(defun zilongshanren/init-wrap-region ()
  (use-package wrap-region
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (java-mode javascript-mode css-mode js2-mode))
         ("`" "`" nil (markdown-mode ruby-mode))))
      (add-to-list 'wrap-region-except-modes 'dired-mode)
      (add-to-list 'wrap-region-except-modes 'web-mode)
      )
    :defer t
    :config
    (spacemacs|hide-lighter wrap-region-mode)))

(defun zilongshanren/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
      (add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
      (add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html
      )))

;; spacemacs distribution disabled this package, because it has overlay bug.
;; I hack the implementation here. on default, the hl-highlight-mode is disabled.
(defun zilongshanren/post-init-hl-anything ()
  (use-package hl-anything
    :init
    (progn
      (hl-highlight-mode -1)
      (spacemacs|add-toggle toggle-hl-anything
        :status hl-highlight-mode
        :on (hl-highlight-mode)
        :off (hl-highlight-mode -1)
        :documentation "Toggle highlight anything mode."
        :evil-leader "ths"))))

(defun zilongshanren/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :init
    (progn
      
      ;; If you use other VCS (subversion, for example), enable the following option
      ;;(setq ffip-project-file ".svn")
      ;; in MacOS X, the search file command is CMD+p
      (bind-key* "s-p" 'find-file-in-project)
      ;; for this project, I'm only interested certain types of files
      ;; (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
      ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
      ;; OR if I'm reading my personal issue track document,
      (defadvice find-file-in-project (before my-find-file-in-project activate compile)
        (when (ffip-current-full-filename-match-pattern-p "\\(/fireball\\)")
          ;; set the root directory into "~/projs/PROJECT_DIR"
          (setq-local ffip-project-root "~/Github/fireball")
          ;; well, I'm not interested in concatenated BIG js file or file in dist/
          (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
          ;; do NOT search files in below directories, the default value is better.
          ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
          )
        (when (ffip-current-full-filename-match-pattern-p "\\(/cocos2d-x\\)")
          ;; set the root directory into "~/projs/PROJECT_DIR"
          (setq-local ffip-project-root "~/cocos2d-x")
          ;; well, I'm not interested in concatenated BIG js file or file in dist/
          (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
          ;; do NOT search files in below directories, the default value is better.
          ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
          ))
      (ad-activate 'find-file-in-project))
    ))

(defun zilongshanren/post-init-org-bullets ()
  (setq org-bullets-bullet-list '("🐉" "🐠" "🐬" "🐤")))

(defun zilongshanren/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

(defun zilongshanren/post-init-hungry-delete ()
  (add-hook 'prog-mode-hook 'hungry-delete-mode))

(defun zilongshanren/post-init-whitespace ()
  (set-face-attribute 'whitespace-tab nil
                      :background "#Adff2f"
                      :foreground "#00a8a8"
                      :weight 'bold)
  (set-face-attribute 'whitespace-trailing nil
                      :background "#e4eeff"
                      :foreground "#183bc8"
                      :weight 'normal)
  (diminish 'whitespace-mode))

(defun zilongshanren/post-init-cc-mode ()
  ;; company backend should be grouped
  (define-key c++-mode-map (kbd "s-.") 'company-ycmd)
  )


(defun zilongshanren/post-init-helm-ag ()
  (setq helm-ag-use-agignore t)
  ;; This settings use .agignore file to ignore items, and it don't respect to .hgignore, .gitignore
  ;; when there are some git repositories are in .gitignore file, this options is very useful.
  ;;And the .agignore file while be searched at PROJECT_ROOT/.agignore and ~/.agignore
  ;; Thanks to 'man ag' and 'customize-group<RET> helm-ag' for finding the solution... Always RTFM.
  (setq helm-ag-command-option " -U" )
  )

(defun zilongshanren/post-init-powerline ()
  (setq powerline-default-separator 'arrow))

(defun zilongshanren/post-init-youdao-dictionary ()
  (evil-leader/set-key "oy" 'youdao-dictionary-search-at-point+))

(defun zilongshanren/post-init-yasnippet ()
  (progn
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))

    (defun zilongshanren/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
          (setq yas-snippet-dirs  my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (spacemacs/add-to-hooks 'zilongshanren/load-yasnippet '(prog-mode-hook
                                                            markdown-mode-hook
                                                            org-mode-hook))
    ))

(defun zilongshanren/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))

    (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (lispy-mode t)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (smartparens-mode t)))
    ))

(defun zilongshanren/post-init-chinese-wbim ()
  (progn
    ;; [[http://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings][keymap - How to override major mode bindings - Emacs Stack Exchange]]
    (bind-key* ";" 'chinese-wbim-insert-ascii)
    (setq chinese-wbim-punc-translate-p nil)
    (spacemacs/declare-prefix "ot" "Toggle")
    (evil-leader/set-key
      "otp" 'chinese-wbim-punc-translate-toggle)
    (setq chinese-wbim-wb-use-gbk t)
    (add-hook 'chinese-wbim-wb-load-hook
              (lambda ()
                (let ((map (chinese-wbim-mode-map)))
                  (define-key map "-" 'chinese-wbim-previous-page)
                  (define-key map "=" 'chinese-wbim-next-page))))
    ))

(defun zilongshanren/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode)))

(defun zilongshanren/post-init-persp-mode ()
  (spacemacs|define-custom-persp "@Cocos2D-X"
                                 :binding "c"
                                 :body
                                 (find-file "~/cocos2d-x/cocos/ui/UIWidget.cpp")
                                 (split-window-right)
                                 (find-file "~/cocos2d-x/cocos/cocos2d.cpp")))

(defun zilongshanren/init-hydra ()
  (use-package hydra
    :init
    (progn
      (when (configuration-layer/package-usedp 'org)
        ;; major mode hydra is really cool, don't need to switch mode anymore
        ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
        ;; If the command will change the buffer, they should be put in these groups.
        ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
        (defhydra hydra-org (:color blue :hint nil)
          "
              ^Org Mode^
--------------------------------------------
          _t_ags   _p_riority _P_roperty
          "
          ("p" org-priority)
          ("t" org-set-tags)
          ("P" org-set-property))
        (require 'org)
        (define-key org-mode-map (kbd "<f2>") 'hydra-org/body)
        (evil-leader/set-key-for-mode 'org-mode
          "." 'hydra-org/body)
        )
      (defhydra hydra-yasnippet (:color blue :hint nil)
        "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
        ("d" yas-load-directory)
        ("e" yas-activate-extra-mode)
        ("i" yas-insert-snippet)
        ("f" yas-visit-snippet-file :color blue)
        ("n" yas-new-snippet)
        ("t" yas-tryout-snippet)
        ("l" yas-describe-tables)
        ("g" yas/global-mode)
        ("m" yas/minor-mode)
        ("a" yas-reload-all))

      ;; (bind-key* "<f3>" 'hydra-yasnippet/body)

      (defhydra hydra-apropos (:color blue)
        "Apropos"
        ("a" apropos "apropos")
        ("c" apropos-command "cmd")
        ("d" apropos-documentation "doc")
        ("e" apropos-value "val")
        ("l" apropos-library "lib")
        ("o" apropos-user-option "option")
        ("u" apropos-user-option "option")
        ("v" apropos-variable "var")
        ("i" info-apropos "info")
        ("t" tags-apropos "tags")
        ("z" hydra-customize-apropos/body "customize"))

      (defhydra hydra-customize-apropos (:color blue)
        "Apropos (customize)"
        ("a" customize-apropos "apropos")
        ("f" customize-apropos-faces "faces")
        ("g" customize-apropos-groups "groups")
        ("o" customize-apropos-options "options"))

      (bind-key*  "<f4>" 'hydra-apropos/body)
      )))

(defun zilongshanren/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (bind-key* "C-s-l" 'mc/edit-lines)
      (bind-key* "C-s-f" 'mc/mark-all-dwim)
      (bind-key* "C-s-." 'mc/mark-next-like-this)
      (bind-key* "C-s-," 'mc/mark-previous-like-this)
      (bind-key* "s->" 'mc/unmark-next-like-this)
      (bind-key* "s-<" 'mc/unmark-previous-like-this)
      (bind-key* "C-c C-s-." 'mc/mark-all-like-this)

      ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
      (define-prefix-command 'endless/mc-map)
      ;; C-x m is usually `compose-mail'. Bind it to something
      ;; else if you use this command.
      (define-key ctl-x-map "m" 'endless/mc-map)
;;; Really really nice!
      (define-key endless/mc-map "i" #'mc/insert-numbers)
      (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
      (define-key endless/mc-map "a" #'mc/mark-all-like-this)

;;; Occasionally useful
      (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
      (define-key endless/mc-map "r" #'mc/reverse-regions)
      (define-key endless/mc-map "s" #'mc/sort-regions)
      (define-key endless/mc-map "l" #'mc/edit-lines)
      (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
      (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines)
      )))

(defun zilongshanren/post-init-helm-gtags ()
  (use-package helm-gtags
    :diminish helm-gtags-mode
    :defer
    :config
    (progn
      (evil-make-overriding-map helm-gtags-mode-map 'normal)
      (add-hook 'helm-gtags-mode-hook #'evil-normalize-keymaps))))

(defun zilongshanren/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :init
    (define-key global-map (kbd "C-c r") 'vr/replace)
    (define-key global-map (kbd "C-c q") 'vr/query-replace)))

(defun zilongshanren/init-visual-regexp ()
  (use-package visual-regexp
    :init))

(defun zilongshanren/init-nodejs-repl ()
  (use-package nodejs-repl
    :init
    (progn 
      (spacemacs/declare-prefix-for-mode 'js2-mode
                                         "me" "evaluating")
      (evil-leader/set-key-for-mode 'js2-mode
        "meb" 'nodejs-repl-eval-buffer))
    :defer t))

(defun zilongshanren/init-flycheck-package ()
  (use-package flycheck-package))

(defun zilongshanren/post-init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (progn
            (setq company-c-headers-path-system
                  (quote
                   ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")))
            (setq company-c-headers-path-user
                  (quote
                   ("/Users/guanghui/cocos2d-x/cocos/platform" "/Users/guanghui/cocos2d-x/cocos" "." "/Users/guanghui/cocos2d-x/cocos/audio/include/"))))))

(defun zilongshanren/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (evil-leader/set-key (kbd "mhm") 'discover-my-major)
      (evilify makey-key-mode makey-key-mode-get-key-map))))

(defun zilongshanren/init-lispy ()
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
      ;; (define-key evil-insert-state-map (kbd "C-y") 'lispy-yank)
      ;; (define-key evil-insert-state-map (kbd "C-d") 'lispy-delete)
      (add-hook 'lispy-mode-hook 'spacemacs/toggle-aggressive-indent-on)
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))))

(defun zilongshanren/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (progn
      (when (configuration-layer/package-usedp 'company)
        (push 'company-dabbrev company-backends-lua-mode)
        (push 'company-etags company-backends-lua-mode))
      (add-hook 'lua-mode-hook 'evil-matchit-mode)
      (add-hook 'lua-mode-hook 'hungry-delete-mode)
      (add-hook 'lua-mode-hook 'smartparens-mode)
      (setq lua-indent-level 4)

      (evil-leader/set-key-for-mode 'lua-mode
        "<tab>" 'hs-toggle-hiding
        "mgg" 'helm-gtags-dwim
        "mgr" 'helm-gtags-find-rtag
        "mgs" 'helm-gtags-find-symbol
        "mgf" 'helm-gtags-find-files))))


(defun zilongshanren/post-init-company ()
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.08)
  (global-set-key (kbd "C-.") 'company-complete)
  (when (configuration-layer/package-usedp 'company)
    (spacemacs|add-company-hook lua-mode)
    (spacemacs|add-company-hook nxml-mode)))


(defun zilongshanren/init-ws-butler ()
  (use-package ws-butler
    :diminish ws-butler-mode
    :init
    (progn
      (add-hook 'c-mode-common-hook 'ws-butler-mode)
      (add-hook 'python-mode-hook 'ws-butler-mode)
      (add-hook 'cython-mode-hook 'ws-butler-mode))))

(defun zilongshanren/init-rtags ()
  (use-package rtags
    :init (require 'company-rtags)
    :config
    (progn
      (evil-leader/set-key-for-mode 'c++-mode
        "mtr" 'rtags-find-references
        "mts" 'rtags-find-symbol
        "mti" 'rtags-imenu
        "mtf" 'rtags-find-file
        "mtv" 'rtags-find-virtuals-at-point)
      )
    ))

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

      (add-hook 'cmake-mode-hook (function cmake-rename-buffer)))))


(defun zilongshanren/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config (progn
              (flycheck-package-setup)
              (setq flycheck-display-errors-function 'flycheck-display-error-messages)
              (setq flycheck-display-errors-delay 0.2)
              ;; (remove-hook 'c-mode-hook 'flycheck-mode)
              ;; (remove-hook 'c++-mode-hook 'flycheck-mode)
              ;; (evilify flycheck-error-list-mode flycheck-error-list-mode-map)
              )))

(defun zilongshanren/post-init-ycmd ()
  (setq ycmd-tag-files 'auto)
  (setq ycmd-request-message-level -1)
  (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/Github/ycmd/ycmd/__main__.py")))
  (setq company-backends-c-mode-common '((company-c-headers
                                          company-dabbrev-code
                                          company-keywords
                                          company-etags
                                          company-gtags :with company-yasnippet)
                                         company-files company-dabbrev )))

;; configs for writing
(defun zilongshanren/post-init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :config
    (progn
      (when (configuration-layer/package-usedp 'company)
        (spacemacs|add-company-hook markdown-mode))
      (defun zilongshanren/markdown-to-html ()
        (interactive)
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name))
        (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

      (evil-leader/set-key-for-mode 'gfm-mode-map
        "mp" 'zilongshanren/markdown-to-html)
      (evil-leader/set-key-for-mode 'markdown-mode
        "mp" 'zilongshanren/markdown-to-html))))

(defun zilongshanren/init-org-octopress ()
  (use-package org-octopress
    :init
    (progn
      (evilify org-octopress-summary-mode org-octopress-summary-mode-map)
      (add-hook 'org-octopress-summary-mode-hook
                #'(lambda () (local-set-key (kbd "q") 'bury-buffer)))
      (setq org-blog-dir "~/4gamers.cn/")
      (setq org-octopress-directory-top org-blog-dir)
      (setq org-octopress-directory-posts (concat org-blog-dir "source/_posts"))
      (setq org-octopress-directory-org-top org-blog-dir)
      (setq org-octopress-directory-org-posts (concat org-blog-dir "blog"))
      (setq org-octopress-setup-file (concat org-blog-dir "setupfile.org"))

      (defun zilongshanren/org-save-and-export ()
        (interactive)
        (org-octopress-setup-publish-project)
        (org-publish-project "octopress" t))

      (evil-leader/set-key "op" 'zilongshanren/org-save-and-export)
      )))

(defun zilongshanren/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn

      (defun zilongshanren-mode-hook ()
        "my web mode hook for HTML REPL"
        (interactive)
        (impatient-mode)
        (httpd-start))

      ;;TODO: should add toggle options here.
      (add-hook 'web-mode-hook 'zilongshanren-mode-hook)
      (evil-leader/set-key-for-mode 'web-mode
        "mp" 'imp-visit-buffer)
      )))

(defun zilongshanren/init-moz-controller ()
  (use-package moz-controller
    :init
    (moz-controller-global-mode t)
    :diminish moz-controller-mode))

(defun zilongshanren/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "andyque")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))


(defun zilongshanren/init-elfeed ()
  (use-package elfeed
    :defer t
    :config
    (progn
      (global-set-key (kbd "C-x w") 'elfeed)

      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsist.com/rss"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"))

      ;; (evilify elfeed-search-mode elfeed-search-mode-map)
      (spacemacs|evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "G" 'elfeed-update
        "g" 'elfeed-search-update--force)

      (defun elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))

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
      (spacemacs/declare-prefix "ol" "command log")
      (evil-leader/set-key
        "oll" 'mwe:log-keyboard-commands
        "olf" 'mwe:open-command-log-buffer))))

(defun zilongshanren/init-swiper ()
  "Initialize my package"
  (use-package swiper
    :init
    (progn
      (setq ivy-display-style 'fancy)

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
          (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))

      (define-key global-map (kbd "C-s") 'swiper)
      (setq ivy-use-virtual-buffers t)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "C-c j") 'counsel-git-grep))))


(defun zilongshanren/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (progn
      (add-to-list 'magit-no-confirm 'stage-all-changes)
      (define-key magit-log-mode-map (kbd "W") 'magit-copy-as-kill)
      (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
      (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
      (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
      (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)

      (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
      ;; (add-hook 'magit-section-set-visibility-hook '(lambda (section) (let ((section-type (magit-section-type section)))
      ;;                                                              (if (or (eq 'untracked section-type)
      ;;                                                                      (eq 'stashes section-type))
      ;;                                                                  'hide))))
      )

    :init
    (progn
      ;; Githu PR settings
      ;; "http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html"
      (setq magit-repository-directories '("~/cocos2d-x/"))
      (setq magit-push-always-verify nil)

      ;; Don't let magit-status mess up window configurations
      ;; http://whattheemacsd.com/setup-magit.el-01.html
      (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))

      (defun magit-quit-session ()
        "Restores the previous window configuration and kills the magit buffer"
        (interactive)
        (kill-buffer)
        (jump-to-register :magit-fullscreen))


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
          (evil-normal-state)))

      (ad-activate 'magit-blame-mode)

      (defadvice git-timemachine-mode (after git-timemachine-change-to-emacs-state activate compile)
        "when entering git-timemachine mode, change evil normal state to emacs state"
        (if (evil-normal-state-p)
            (evil-emacs-state)
          (evil-normal-state)))

      (ad-activate 'git-timemachine-mode)

      (setq magit-process-popup-time 10))))

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

      (define-key git-messenger-map (kbd "f") 'my-vc-visit-file-revision))))

(defun zilongshanren/post-init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init
    ;; "http://emacs.stackexchange.com/questions/14909/how-to-use-flyspell-to-efficiently-correct-previous-word/14912#14912"
    (defun zilongshanren/flyspell-goto-previous-error (arg)
      "Go to arg previous spelling error."
      (interactive "p")
      (while (not (= 0 arg))
        (let ((pos (point))
              (min (point-min)))
          (if (and (eq (current-buffer) flyspell-old-buffer-error)
                   (eq pos flyspell-old-pos-error))
              (progn
                (if (= flyspell-old-pos-error min)
                    ;; goto beginning of buffer
                    (progn
                      (message "Restarting from end of buffer")
                      (goto-char (point-max)))
                  (backward-word 1))
                (setq pos (point))))
          ;; seek the next error
          (while (and (> pos min)
                      (let ((ovs (overlays-at pos))
                            (r '()))
                        (while (and (not r) (consp ovs))
                          (if (flyspell-overlay-p (car ovs))
                              (setq r t)
                            (setq ovs (cdr ovs))))
                        (not r)))
            (backward-word 1)
            (setq pos (point)))
          ;; save the current location for next invocation
          (setq arg (1- arg))
          (setq flyspell-old-pos-error pos)
          (setq flyspell-old-buffer-error (current-buffer))
          (goto-char pos)
          (call-interactively 'helm-flyspell-correct)
          (if (= pos min)
              (progn
                (message "No more miss-spelled word!")
                (setq arg 0))))))

    (bind-key* "C-;" 'zilongshanren/flyspell-goto-previous-error)
    (global-set-key (kbd "C-c s") 'helm-flyspell-correct)))

(defun zilongshanren/post-init-helm ()
  (use-package helm
    :init
    (progn
      (global-set-key (kbd "C-s-y") 'helm-show-kill-ring)
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
              (read-directory-name . ido))))))



(defun zilongshanren/post-init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (global-set-key (kbd "C-x C-o") #'ace-window)))

(defun zilongshanren/post-init-avy ()
  (use-package avy
    :defer t
    :init
    (progn
      (require 'ace-pinyin)
      (setq ace-pinyin-use-avy t)
      (global-set-key (kbd "C-s-'") 'avy-goto-char-2))))

(defun zilongshanren/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :init
    :config
    (progn
      ;;beautify-helm buffer when long file name is present
      (setq helm-buffer-max-length 45)
      (setq helm-ls-git-show-abs-or-relative 'relative))))


;;configs for EVIL mode
(defun zilongshanren/post-init-evil ()
  (use-package evil
    :init
    (progn
      ;; make underscore as word_motion.
      (modify-syntax-entry ?_ "w")
      ;; ;; change evil initial mode state
      (loop for (mode . state) in
            '((shell-mode . normal))
            do (evil-set-initial-state mode state))

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

      (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
      (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))


      (define-key evil-normal-state-map (kbd "[ b") 'spacemacs/previous-useful-buffer)
      (define-key evil-normal-state-map (kbd "] b") 'spacemacs/next-useful-buffer)

      ;; (define-key evil-insert-state-map "\C-e" 'end-of-line)
      ;; (define-key evil-insert-state-map "\C-n" 'next-line)
      ;; (define-key evil-insert-state-map "\C-k" 'kill-line)
      (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
      (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)

      (evil-leader/set-key "bi" 'ibuffer)
      (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
      (define-key evil-ex-completion-map "\C-b" 'backward-char)
      (define-key evil-ex-completion-map "\C-k" 'kill-line)
      (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

      (define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
      (define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
      (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
      ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
      ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
      (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)

      ;; in spacemacs, we always use evilify miscro state
      (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

      ;; (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
      (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
      ;; (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
      ;; (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
      ;; (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

      ;; for emacs shell mode
      ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
      ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
      (evil-define-key 'emacs term-raw-map (kbd "C-w")
        'evil-delete-backward-word)

      (evil-leader/set-key "fR" 'zilongshanren/rename-file-and-buffer)
      (evil-leader/set-key "bms" 'bookmark-set)
      (evil-leader/set-key "bmr" 'bookmark-rename)
      (evil-leader/set-key "bmd" 'bookmark-delete)

      ;; enable hybrid editing style
      (defadvice evil-insert-state (around zilongshanren/holy-mode activate)
        "Preparing the holy water flasks."
        (evil-emacs-state))
      ;; disable c-[ temporally
      ;; (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
      ;; (bind-keys ("<C-[>" . evil-normal-state))
      ;; (setq evil-emacs-state-cursor '("chartreuse3" (bar . 2)))
      (define-key evil-emacs-state-map [escape] 'evil-normal-state)


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
      (setq org-reveal-root "file:///Users/guanghui/.emacs.d/reveal-js"))))

(defun zilongshanren/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)))

(defun zilongshanren/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (org-download-enable)))

(defun zilongshanren/post-init-org-pomodoro ()
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))

;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun zilongshanren/post-init-org ()
  (progn
    ;; https://github.com/syl20bnr/spacemacs/issues/2994#issuecomment-139737911
    ;; (when (configuration-layer/package-usedp 'company)
    ;;   (spacemacs|add-company-hook org-mode))
    (spacemacs|disable-company org-mode)

    (require 'org-compat)
    (require 'org)
    ;; (add-to-list 'org-modules "org-habit")
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; define the refile targets
    (setq org-agenda-files (quote ("~/org-notes" )))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    ;; config stuck project
    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-agenda-inhibit-startup t)       ;; ~50x speedup
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)
    (setq org-log-done t)

    ;; 加密文章
    ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
    ;; org-mode 設定
    (require 'org-crypt)

    ;; 當被加密的部份要存入硬碟時，自動加密回去
    (org-crypt-use-before-save-magic)

    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "secret")

    ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))

    ;; 用於加密的 GPG 金鑰
    ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
    (setq org-crypt-key nil)

    (add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))

    (setq org-mobile-directory "~/org-notes/org")


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)"  "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line


    (setq org-default-notes-file "~/org-notes/gtd.org")

    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/org-notes/gtd.org" "Daily Tasks")
             "* TODO %?\n  %i\n"
             :empty-lines 1)
            ("n" "notes" entry (file+headline "~/org-notes/notes.org" "Quick notes")
             "* TODO [#C] %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline "~/org-notes/notes.org" "Blog Ideas")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("w" "work" entry (file+headline "~/org-notes/gtd.org" "Cocos2D-X")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("c" "Chrome" entry (file+headline "~/org-notes/notes.org" "Quick notes")
             "* TODO %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
             :empty-lines 1)
            ("l" "links" entry (file+headline "~/org-notes/notes.org" "Quick notes")
             "* TODO %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree "~/org-notes/journal.org")
             "* %?"
             :empty-lines 1)))

    (setq org-tags-match-list-sublevels nil)
    ;;An entry without a cookie is treated just like priority ' B '.
    ;;So when create new task, they are default 重要且紧急
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
            ("W" "Weekly Review"
             ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
    
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
    ;; used by org-clock-sum-today-by-tags
    (defun filter-by-tags ()
      (let ((head-tags (org-get-tags-at)))
        (member current-tag head-tags)))

    (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
      (interactive "P")
      (let* ((timerange-numeric-value (prefix-numeric-value timerange))
             (files (org-add-archive-files (org-agenda-files)))
             (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                             "LIFE" "PROJECT" "OTHER"))
             (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
             (output-string "")
             (tstart (or tstart
                         (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                         (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                         (org-time-today)))
             (tend (or tend
                       (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                       (+ tstart 86400)))
             h m file item prompt donesomething)
        (while (setq file (pop files))
          (setq org-agenda-buffer (if (file-exists-p file)
                                      (org-get-agenda-file-buffer file)
                                    (error "No such file %s" file)))
          (with-current-buffer org-agenda-buffer
            (dolist (current-tag include-tags)
              (org-clock-sum tstart tend 'filter-by-tags)
              (setcdr (assoc current-tag tags-time-alist)
                      (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
        (while (setq item (pop tags-time-alist))
          (unless (equal (cdr item) 0)
            (setq donesomething t)
            (setq h (/ (cdr item) 60)
                  m (- (cdr item) (* 60 h)))
            (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
        (unless donesomething
          (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
        (unless noinsert
          (insert output-string))
        output-string))


    ;; http://wenshanren.org/?p=327
    ;; change it to helm
    (defun zilongshanren/org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
                "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
                "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
                "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                "scheme" "sqlite")))
         (list (ido-completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (add-hook 'org-mode-hook '(lambda ()
                                ;; keybinding for editing source code blocks
                                ;; keybinding for inserting code blocks
                                (local-set-key (kbd "C-c i s")
                                               'zilongshanren/org-insert-src-block)
                                ))
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

    ;; {{ export org-mode in Chinese into PDF
    ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
    ;; and you need install texlive-xetex on different platforms
    ;; To install texlive-xetex:
    ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
    ;; }}
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
        (js . t)
        (python . t)
        (emacs-lisp . t)
        (plantuml . t)
        (C . t)
        (R . t)
        (ditaa . t)))

    (setq org-plantuml-jar-path
          (expand-file-name "~/.spacemacs.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")


    (defvar zilongshanren-website-html-preamble
      "<div class='nav'>
<ul>
<li><a href='http://zilongshanren.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
    (defvar zilongshanren-website-html-blog-head
      " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
    (setq org-publish-project-alist
          `(
            ("blog-notes"
             :base-directory "~/org-notes"
             :base-extension "org"
             :publishing-directory "~/org-notes/public_html/"

             :recursive t
             :html-head , zilongshanren-website-html-blog-head
             :publishing-function org-html-publish-to-html
             :headline-levels 4         ; Just the default for this project.
             :auto-preamble t
             :exclude "gtd.org"
             :exclude-tags ("ol" "noexport")
             :section-numbers nil
             :html-preamble ,zilongshanren-website-html-preamble
             :author "zilongshanren"
             :email "guanghui8827@gmail.com"
             :auto-sitemap t               ; Generate sitemap.org automagically...
             :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
             :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
             :sitemap-sort-files anti-chronologically
             :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
             )
            ("blog-static"
             :base-directory "~/org-notes"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory "~/org-notes/public_html/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("blog" :components ("blog-notes" "blog-static"))))


    (global-set-key (kbd "C-c a") 'org-agenda)
    (define-key org-mode-map (kbd "s-p") 'org-priority)
    (define-key global-map (kbd "<f9>") 'org-capture)
    (global-set-key (kbd "C-c b") 'org-iswitchb)
    (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
    (evil-leader/set-key-for-mode 'org-mode
      "owh" 'plain-org-wiki-helm
      "owf" 'plain-org-wiki)

    ))

(defun zilongshanren/post-init-deft ()
  (setq deft-use-filter-string-for-filename t)
  (evil-leader/set-key-for-mode 'deft-mode "mq" 'quit-window)
  (setq deft-extension "org")
  (setq deft-directory "~/org-notes"))



(defun zilongshanren/post-init-prodigy ()
  (prodigy-define-tag
    :name 'jekyll
    :env '(("LANG" "en_US.UTF-8")
           ("LC_ALL" "en_US.UTF-8")))
  ;; define service
  (prodigy-define-service
    :name "Preview Cocos2D-HTML5"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6001")
    :cwd "~/cocos2d-x/web"
    :tags '(work)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Server"
    :command "hexo"
    :args '("server")
    :cwd "~/4gamers.cn"
    :tags '(hexo server)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Deploy"
    :command "hexo"
    :args '("deploy" "--generate")
    :cwd "~/4gamers.cn"
    :tags '(hexo deploy)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Debug Fireball"
    :command "gulp"
    :args '("fireball" "--path" "/Users/guanghui/workspace/fireball/HelloFireBall/")
    :cwd "~/Github/fireball/"
    :tags '(work)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Org wiki preview"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "8088")
    :cwd "~/org-notes/public_html"
    :tags '(org-mode)
    :init (lambda () (browse-url "http://localhost:8088"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t))

(defun zilongshanren/post-init-js2-mode ()
  (progn
    (setq company-backends-js2-mode '((company-dabbrev-code
                                       company-keywords
                                       company-etags) company-files company-dabbrev))
    ;; (remove-hook 'js2-mode-hook 'flycheck-mode)
    (defun conditional-disable-modes ()
      (when (> (buffer-size) 1000000)
        (flycheck-mode -1)))

    (add-hook 'js2-mode-hook 'conditional-disable-modes)

    (use-package js2-mode
      :defer t
      :config
      (progn
        (define-key js2-mode-map   (kbd "s-.") 'company-tern)
        ;; these mode related variables must be in eval-after-load
        ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
        (setq-default js2-allow-rhino-new-expr-initializer nil)
        (setq-default js2-auto-indent-p nil)
        (setq-default js2-enter-indents-newline nil)
        (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq-default js2-idle-timer-delay 0.1)
        (setq-default js2-mirror-mode nil)
        (setq-default js2-strict-inconsistent-return-warning nil)
        (setq-default js2-include-rhino-externs nil)
        (setq-default js2-include-gears-externs nil)
        (setq-default js2-concat-multiline-strings 'eol)
        (setq-default js2-rebind-eol-bol-keys nil)
        (setq-default js2-auto-indent-p t)

        (setq-default js2-bounce-indent nil)
        (setq-default js-indent-level 2)
        (setq-default js2-basic-offset 2)
        ;; Let flycheck handle parse errors
        (setq-default js2-show-parse-errors nil)
        (setq-default js2-strict-missing-semi-warning nil)

        (autoload 'flycheck-get-checker-for-buffer "flycheck")
        (defun sanityinc/disable-js2-checks-if-flycheck-active ()
          (unless (flycheck-get-checker-for-buffer)
            (set (make-local-variable 'js2-mode-show-parse-errors) t)
            (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
        (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
        
        (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))))

    (evilify js2-error-buffer-mode js2-error-buffer-mode-map)


    (evil-leader/set-key-for-mode 'js2-mode
      "med" 'nodejs-repl-eval-dwim)

    (evil-leader/set-key-for-mode 'js2-mode
      "mga" 'projectile-find-other-file
      "mgA" 'projectile-find-other-file-other-window)

    (evil-leader/set-key-for-mode 'web-mode
      "mga" 'projectile-find-other-file
      "mgA" 'projectile-find-other-file-other-window)


    (defun js2-imenu-make-index ()
      (save-excursion
        ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
        (imenu--generic-function '(("describe" "\\s-*describe\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("it" "\\s-*it\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("before" "\\s-*before\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("after" "\\s-*after\\s-*([\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                                   ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                                   ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                                   ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                                   ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
                                   ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
                                   ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
                                   ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                   ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ))))

    (add-hook 'js2-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'js2-imenu-make-index)))
    ))

(defun zilongshanren/post-init-popwin ()
  (progn
    (push "*zilongshanren/run-current-file output*" popwin:special-display-config)
    (delete "*Async Shell Command*" 'popwin:special-display-config)
    ))

(defun zilongshanren/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun zilongshanren/post-init-tagedit ()
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))
