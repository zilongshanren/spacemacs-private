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
     better-defaults
     github
     ranger
     colors
     prodigy
     search-engine
     graphviz
     (syntax-checking :variables syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     (vinegar :variables vinegar-reuse-dired-buffer t)
     (spacemacs-layouts :variables layouts-enable-autosave nil
                        layouts-autosave-delay 300)
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (auto-completion :variables auto-completion-enable-sort-by-usage t
                      :disabled-for org markdown)
     (osx :variables osx-dictionary-dictionary-choice "Simplified Chinese - English")
     restclient
     (gtags :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     (shell :variables shell-default-shell 'eshell)
     docker
     latex
     deft
     markdown
     org
     shaders
     yaml
     react
     (python :variables
             python-test-runner '(nose pytest))
     (ruby :variables ruby-enable-enh-ruby-mode t
           ruby-version-manager 'chruby)
     ruby-on-rails
     lua
     html
     javascript
     (typescript :variables
                 typescript-fmt-on-save nil
                 typescript-fmt-tool 'typescript-formatter)
     emacs-lisp
     (clojure :variables clojure-enable-fancify-symbols t)
     racket
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     zilongshanren
     (chinese :packages youdao-dictionary fcitx
              :variables chinese-enable-fcitx t
              chinese-enable-youdao-dict t)
     )
   dotspacemacs-additional-packages '(sicp)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   '(counsel-projectile magit-gh-pulls magit-gitflow org-projectile evil-mc
                        evil-args evil-ediff evil-exchange evil-unimpaired
                        evil-indent-plus volatile-highlights
                        spaceline holy-mode skewer-mode rainbow-delimiters
                        highlight-indentation vi-tilde-fringe eyebrowse
                        org-bullets smooth-scrolling org-repo-todo org-download org-timer
                        livid-mode git-gutter git-gutter-fringe  evil-escape
                        leuven-theme gh-md evil-lisp-state spray lorem-ipsum
                        ac-ispell ace-jump-mode auto-complete auto-dictionary
                        clang-format define-word google-translate disaster epic
                        fancy-battery neotree org-present orgit orglue spacemacs-theme
                        helm-flyspell flyspell-correct-helm clean-aindent-mode
                        helm-c-yasnippet ace-jump-helm-line helm-make
                        helm-themes helm-swoop helm-spacemacs-help smeargle
                        ido-vertical-mode flx-ido company-quickhelp)
   dotspacemacs-install-packages 'used-only
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
        '(("melpa-cn" . "https://elpa.zilongshanren.com/melpa/")
          ("org-cn"   . "https://elpa.zilongshanren.com/org/")
          ("gnu-cn"   . "https://elpa.zilongshanren.com/gnu/")))

  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  ;; (setq tramp-mode nil)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; ss proxy. But it will cause anacond-mode failed.
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (setq evil-shift-round nil)
  (setq byte-compile-warnings '(not obsolete))
  (setq warning-minimum-level :error)
  )

(defun dotspacemacs/user-config ()
  ;;解决org表格里面中英文对齐的问题
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-mac) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))

  ;; Setting Chinese Font
  (when (and (spacemacs/system-is-mswindows) window-system)
    (setq ispell-program-name "aspell")
    (setq w32-pass-alt-to-system nil)
    (setq w32-apps-modifier 'super)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Microsoft Yahei" :size 14))))

  (fset 'evil-visual-update-x-selection 'ignore)

  ;; force horizontal split window
  (setq split-width-threshold 120)
  (linum-relative-on)

  (spacemacs|add-company-hook 'text-mode)

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; temp fix for ivy-switch-buffer
  ;; (spacemacs/set-leader-keys "bb" 'helm-mini)

  (global-hungry-delete-mode t)
  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish ggtags-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish spacemacs-whitespace-cleanup-mode)

  ;; https://emacs-china.org/t/ranger-golden-ratio/964/2
  (defun my-ranger ()
    (interactive)
    (if golden-ratio-mode
        (progn
          (golden-ratio-mode -1)
          (ranger)
          (setq golden-ratio-previous-enable t))
      (progn
        (ranger)
        (setq golden-ratio-previous-enable nil))))

  (defun my-quit-ranger ()
    (interactive)
    (if golden-ratio-previous-enable
        (progn
          (ranger-close)
          (golden-ratio-mode 1))
      (ranger-close)))

  (with-eval-after-load 'ranger
    (progn
      (define-key ranger-normal-mode-map (kbd "q") 'my-quit-ranger)))

  (spacemacs/set-leader-keys "ar" 'my-ranger)

  (when (configuration-layer/layer-usedp 'ivy)
    (setq projectile-switch-project-action
          'zilongshanren/open-file-with-projectile-or-counsel-git))

  (evilified-state-evilify-map special-mode-map :mode special-mode)

  (add-to-list 'auto-mode-alist
               '("Capstanfile\\'" . yaml-mode))

  (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on))

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
