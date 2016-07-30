(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'nil
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     helm
     ivy
     ;; react
     better-defaults
     github
     osx
     ;; latex
     deft
     markdown
     (vinegar :variables vinegar-reuse-dired-buffer t)
     org
     prodigy
     search-engine
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     yaml
     ;; (ruby :variables ruby-version-manager 'rvm)
     (python :variables
             python-test-runner '(nose pytest))
     lua
     html
     ;; command-log
     javascript
     (typescript :variables
                 typescript-fmt-on-save nil
                 typescript-fmt-tool 'typescript-formatter)
     ;; restclient
     emacs-lisp
     (clojure :variables clojure-enable-fancify-symbols t)
     ranger
     ;; racket
     (spacemacs-layouts :variables layouts-enable-autosave nil
                        layouts-autosave-delay 300)
     colors
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     (auto-completion :variables auto-completion-enable-sort-by-usage t
                      :disabled-for org markdown)
     zilongshanren
     )
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   '(counsel-projectile magit-gh-pulls magit-gitflow org-projectile evil-mc
                        evil-args evil-ediff evil-exchange evil-unimpaired
                        evil-indent-plus centered-buffer-mode volatile-highlights
                        smartparens spaceline holy-mode skewer-mode
                        highlight-indentation vi-tilde-fringe eyebrowse hl-anything
                        org-bullets smooth-scrolling org-repo-todo org-download org-timer
                        livid-mode git-gutter git-gutter-fringe alert evil-escape
                        leuven-theme gh-md evil-lisp-state spray doc-view lorem-ipsum
                        ac-ispell ace-jump-mode auto-complete auto-dictionary
                        clang-format define-word google-translate disaster epic
                        fancy-battery neotree org-present orgit orglue spacemacs-theme
                        spinner tagedit helm-flyspell flyspell-correct-helm
                        helm-c-yasnippet ace-jump-helm-line helm-make helm-projectile
                        helm-themes helm-swoop helm-spacemacs-help)
   dotspacemacs-download-packages 'used
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(solarized-light solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key ":"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "http://elpa.zilongshanren.com/melpa/")
          ("org-cn"   . "http://elpa.zilongshanren.com/org/")
          ("gnu-cn"   . "http://elpa.zilongshanren.com/gnu/")))
 
  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  ;; (setq tramp-mode nil)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; ss proxy. But it will cause anacond-mode failed.
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (setq evil-shift-round nil)
  (setq byte-compile-warnings '(not obsolete))
  )

(defun dotspacemacs/user-config ()
  ;;解决org表格里面中英文对齐的问题
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-mac) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))


  (spacemacs|add-company-hook 'text-mode)

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; temp fix for ivy-switch-buffer
  (spacemacs/set-leader-keys "bb" 'helm-mini)

  (global-hungry-delete-mode t)

  (when (configuration-layer/layer-usedp 'ivy)
    (setq projectile-switch-project-action
          'zilongshanren/open-file-with-projectile-or-counsel-git))

  ;; visual line mode will cause swiper slow...
  ;; (add-hook 'prog-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on))

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
