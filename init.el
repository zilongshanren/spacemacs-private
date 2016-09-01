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
     latex
     deft
     markdown
     org
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
     )
   dotspacemacs-additional-packages '(sicp)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   '(counsel-projectile magit-gh-pulls magit-gitflow org-projectile evil-mc
                        evil-args evil-ediff evil-exchange evil-unimpaired
                        evil-indent-plus volatile-highlights
                        spaceline holy-mode skewer-mode rainbow-delimiters
                        highlight-indentation vi-tilde-fringe eyebrowse hl-anything
                        org-bullets smooth-scrolling org-repo-todo org-download org-timer
                        livid-mode git-gutter git-gutter-fringe  evil-escape
                        leuven-theme gh-md evil-lisp-state spray lorem-ipsum
                        ac-ispell ace-jump-mode auto-complete auto-dictionary
                        clang-format define-word google-translate disaster epic
                        fancy-battery neotree org-present orgit orglue spacemacs-theme
                        helm-flyspell flyspell-correct-helm clean-aindent-mode
                        helm-c-yasnippet ace-jump-helm-line helm-make helm-projectile
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

  (defun read-multiple-choice (prompt choices)
    "Ask user a multiple choice question.
PROMPT should be a string that will be displayed as the prompt.
CHOICES is an alist where the first element in each entry is a
character to be entered, the second element is a short name for
the entry to be displayed while prompting (if there's room, it
might be shortened), and the third, optional entry is a longer
explanation that will be displayed in a help buffer if the user
requests more help.
This function translates user input into responses by consulting
the bindings in `query-replace-map'; see the documentation of
that variable for more information.  In this case, the useful
bindings are `recenter', `scroll-up', and `scroll-down'.  If the
user enters `recenter', `scroll-up', or `scroll-down' responses,
perform the requested window recentering or scrolling and ask
again.
The return value is the matching entry from the CHOICES list.
Usage example:
\(read-multiple-choice \"Continue connecting?\"
                      '((?a \"always\")
                        (?s \"session only\")
                        (?n \"no\")))"
    (let* ((altered-names nil)
           (full-prompt
            (format
             "%s (%s): "
             prompt
             (mapconcat
              (lambda (elem)
                (let* ((name (cadr elem))
                       (pos (seq-position name (car elem)))
                       (altered-name
                        (cond
                         ;; Not in the name string.
                         ((not pos)
                          (format "[%c] %s" (car elem) name))
                         ;; The prompt character is in the name, so highlight
                         ;; it on graphical terminals...
                         ((display-supports-face-attributes-p
                           '(:underline t) (window-frame))
                          (setq name (copy-sequence name))
                          (put-text-property pos (1+ pos)
                                             'face 'read-multiple-choice-face
                                             name)
                          name)
                         ;; And put it in [bracket] on non-graphical terminals.
                         (t
                          (concat
                           (substring name 0 pos)
                           "["
                           (upcase (substring name pos (1+ pos)))
                           "]"
                           (substring name (1+ pos)))))))
                  (push (cons (car elem) altered-name)
                        altered-names)
                  altered-name))
              (append choices '((?? "?")))
              ", ")))
           tchar buf wrong-char answer)
      (save-window-excursion
        (save-excursion
          (while (not tchar)
            (message "%s%s"
                     (if wrong-char
                         "Invalid choice.  "
                       "")
                     full-prompt)
            (setq tchar
                  (if (and (display-popup-menus-p)
                           last-input-event ; not during startup
                           (listp last-nonmenu-event)
                           use-dialog-box)
                      (x-popup-dialog
                       t
                       (cons prompt
                             (mapcar
                              (lambda (elem)
                                (cons (capitalize (cadr elem))
                                      (car elem)))
                              choices)))
                    (condition-case nil
                        (let ((cursor-in-echo-area t))
                          (read-char))
                      (error nil))))
            (setq answer (lookup-key query-replace-map (vector tchar) t))
            (setq tchar
                  (cond
                   ((eq answer 'recenter)
                    (recenter) t)
                   ((eq answer 'scroll-up)
                    (ignore-errors (scroll-up-command)) t)
                   ((eq answer 'scroll-down)
                    (ignore-errors (scroll-down-command)) t)
                   ((eq answer 'scroll-other-window)
                    (ignore-errors (scroll-other-window)) t)
                   ((eq answer 'scroll-other-window-down)
                    (ignore-errors (scroll-other-window-down)) t)
                   (t tchar)))
            (when (eq tchar t)
              (setq wrong-char nil
                    tchar nil))
            ;; The user has entered an invalid choice, so display the
            ;; help messages.
            (when (and (not (eq tchar nil))
                       (not (assq tchar choices)))
              (setq wrong-char (not (memq tchar '(?? ?\C-h)))
                    tchar nil)
              (when wrong-char
                (ding))
              (with-help-window (setq buf (get-buffer-create
                                           "*Multiple Choice Help*"))
                (with-current-buffer buf
                  (erase-buffer)
                  (pop-to-buffer buf)
                  (insert prompt "\n\n")
                  (let* ((columns (/ (window-width) 25))
                         (fill-column 21)
                         (times 0)
                         (start (point)))
                    (dolist (elem choices)
                      (goto-char start)
                      (unless (zerop times)
                        (if (zerop (mod times columns))
                            ;; Go to the next "line".
                            (goto-char (setq start (point-max)))
                          ;; Add padding.
                          (while (not (eobp))
                            (end-of-line)
                            (insert (make-string (max (- (* (mod times columns)
                                                            (+ fill-column 4))
                                                         (current-column))
                                                      0)
                                                 ?\s))
                            (forward-line 1))))
                      (setq times (1+ times))
                      (let ((text
                             (with-temp-buffer
                               (insert (format
                                        "%c: %s\n"
                                        (car elem)
                                        (cdr (assq (car elem) altered-names))))
                               (fill-region (point-min) (point-max))
                               (when (nth 2 elem)
                                 (let ((start (point)))
                                   (insert (nth 2 elem))
                                   (unless (bolp)
                                     (insert "\n"))
                                   (fill-region start (point-max))))
                               (buffer-string))))
                        (goto-char start)
                        (dolist (line (split-string text "\n"))
                          (end-of-line)
                          (if (bolp)
                              (insert line "\n")
                            (insert line))
                          (forward-line 1)))))))))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (assq tchar choices)))

  (when (configuration-layer/layer-usedp 'ivy)
    (setq projectile-switch-project-action
          'zilongshanren/open-file-with-projectile-or-counsel-git))

  ;; visual line mode will cause swiper slow...
  ;; (add-hook 'prog-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on))

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
