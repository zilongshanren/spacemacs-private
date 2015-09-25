;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `!distribution'. For now available distributions are `spacemacs-core'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
        ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;;  Guide key
     ;; --------------------------------------------------------
     better-defaults
     github
     version-control
     osx
     semantic                           ; too slow
     markdown
     vim-empty-lines
     vinegar
     ;; (ruby :variables ruby-version-manager 'rvm)
     org
     ;; dash
     prodigy
     search-engine
     (syntax-checking :variables syntax-checking-enable-tooltips t)
     spell-checking
     yaml
     ;; python
     html
     javascript
     ;; restclient
     emacs-lisp
     ;; emoji
     racket
     gtags
     ;; deft
     ;; lua
     ;; (clojure :variables clojure-enable-fancify-symbols t)
     ;; perspectives
     eyebrowse
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     (git :variables
          git-magit-status-fullscreen t)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     (auto-completion)
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-term-shell "/bin/zsh")
     (chinese :variables chinese-default-input-method 'wubi
              chinese-enable-youdao-dict t)
     zilongshanren
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(magit-gh-pulls
                                    magit-gitflow
                                    magit-svn
                                    ;;remove from spacemacs distribution
                                    ;; neotree
                                    leuven-theme
                                    evil-lisp-state
                                    spray
                                    evil-tutor
                                    define-word
                                    solarized-theme
                                    ;; remove mode for python layer
                                    nose
                                    pony-mode
                                    hy-mode)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'doge
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects bookmarks)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         solarized-light
                         leuven
                         ;; sanityinc-tomorrow-day
                         ;; sanityinc-tomorrow-eighties
                         ;; spacemacs-dark
                         ;; spacemacs-light
                         ;; solarized-dark
                         ;; zenburn
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  ;; ss proxy. But it will cause anacond-mode failed.
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  ;;I have changed this settings to shadowsocks minor mode
  ;; (setq url-gateway-method 'socks)
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  (add-hook 'prog-mode-hook #'linum-mode)
  (with-eval-after-load 'linum
    (linum-relative-toggle))

  ;;解决org表格里面中英文对齐的问题
  (when (configuration-layer/layer-usedp 'chinese)
    (when (spacemacs/system-is-mac)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))

  (global-company-mode t)

  ;;currently this key map must be put in user-config to override the defaults
  (evil-leader/set-key "pf" 'helm-ls-git-ls)


  (defun conditional-disable-modes ()
    (when (> (buffer-size) 1000000)
      (flycheck-mode -1)))

  (add-hook 'js2-mode-hook 'conditional-disable-modes)

  (when (configuration-layer/layer-usedp 'vinegar)
    (evilify dired-mode dired-mode-map
             (kbd "C-k") 'zilongshanren/dired-up-directory))

  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev))

  ;; insert ; at the end of current line
  (defun zilongshanren/insert-semicolon-at-the-end-of-this-line ()
    (interactive)
    (save-excursion
      (end-of-line)
      (insert ";")))
  (bind-key* "s-;" 'zilongshanren/insert-semicolon-at-the-end-of-this-line)

  )
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(cfs--current-profile-name "profile1" t)
 '(expand-region-contract-fast-key "V")
 '(expand-region-exclude-text-mode-expansions (quote (html-mode nxml-mode web-mode)))
 '(expand-region-reset-fast-key "r")
 '(golden-ratio-exclude-modes
   (quote
    ("bs-mode" "calc-mode" "ediff-mode" "dired-mode" "gud-mode" "gdb-locals-mode" "gdb-registers-mode" "gdb-breakpoints-mode" "gdb-threads-mode" "gdb-frames-mode" "gdb-inferior-io-mode" "gud-mode" "gdb-inferior-io-mode" "gdb-disassembly-mode" "gdb-memory-mode" "restclient-mode" "speedbar-mode" term-mode)))
 '(helm-ag-always-set-extra-option nil)
 '(helm-ls-git-fuzzy-match t)
 '(helm-ls-git-show-abs-or-relative (quote absolute))
 '(lua-documentation-url "http://www.lua.org/manual/5.3/manual.html")
 '(magit-use-overlays nil)
 '(org-agenda-custom-commands nil)
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (edn paredit queue peg json-rpc dash-functional web-completion-data makey anzu highlight goto-chg flx gh logito pcache pos-tip guide-key request parent-mode simple-httpd json-snatcher json-reformat multiple-cursors moz ctable orglue epic alert log4e gntp spinner epl hydra async deferred f s chinese-word-at-point dash youdao-dictionary ws-butler window-numbering web-mode web-beautify volatile-highlights vi-tilde-fringe use-package tagedit smooth-scrolling slim-mode scss-mode sass-mode rfringe reveal-in-finder rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pyenv-mode popwin pip-requirements persp-projectile pbcopy page-break-lines ox-reveal org-repo-todo org-present org-octopress org-mac-link org-download org-bullets open-junk-file neotree multi-term moz-controller move-text monokai-theme markdown-toc magit macrostep lispy linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc indent-guide impatient-mode ido-vertical-mode hungry-delete hl-anything highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-ls-git helm-gtags helm-gitignore helm-github-stars helm-flyspell helm-descbinds helm-css-scss helm-c-yasnippet helm-ag guide-key-tip google-translate golden-ratio github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh-md ggtags geiser fringe-helper flycheck-ycmd flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-tutor evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-args evil-anzu engine-mode emmet-mode elisp-slime-nav elfeed discover-my-major deft dash-at-point cython-mode company-ycmd company-web company-tern company-statistics company-quickhelp company-c-headers company-anaconda command-log-mode coffee-mode cmake-font-lock clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu chinese-fonts-setup buffer-move auto-yasnippet auto-highlight-symbol auto-dictionary align-cljlet aggressive-indent adaptive-wrap ace-jump-mode ac-ispell 2048-game)))
 '(paradox-github-token t)
 '(pyim-dicts
   (quote
    ((:name "BigDict-01" :file "/Users/guanghui/.emacs.d/pyim/dicts/pyim-bigdict.pyim" :coding utf-8-unix)
     (:name "BigDict-01" :file "/Users/guanghui/.emacs.d/.cache/pyim-bigdict.pyim" :coding utf-8-unix))))
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((eval setenv "PYTHONPATH" "/Users/guanghui/cocos2d-x/tools/cocos2d-console/plugins:/Users/guanghui/cocos2d-x/tools/cocos2d-console/bin"))))
 '(sp-show-pair-from-inside t)
 '(vc-follow-symlinks t)
 '(ycmd-extra-conf-handler (quote load))
 '(ycmd-extra-conf-whitelist (quote ("~/cocos2d-x/*"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "green"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(git-gutter-fr:added ((t (:foreground "#859900" :weight bold :width extra-expanded))))
 '(helm-ls-git-modified-and-staged-face ((t (:foreground "dark cyan"))))
 '(helm-ls-git-modified-not-staged-face ((t (:foreground "dark cyan"))))
 '(helm-ls-git-renamed-modified-face ((t (:foreground "dark cyan")))))
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
