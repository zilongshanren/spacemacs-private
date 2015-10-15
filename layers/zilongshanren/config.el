;;; config.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|defvar-company-backends lua-mode)
(spacemacs|defvar-company-backends markdown-mode)
(spacemacs|defvar-company-backends org-mode)
(spacemacs|defvar-company-backends nxml-mode)

(spacemacs|add-toggle iimage
                      :status iimage-mode
                      :on (iimage-mode)
                      :off (iimage-mode -1)
                      :documentation "Enable iimage mode"
                      :evil-leader "Ti")

(add-hook 'prog-mode-hook 'spacemacs/highlight-TODO-words)
(require 'dired-x)
(require 'dired-aux)

(setq dired-listing-switches "-alh")
(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "open")
        ("\\.docx\\'" "open")
        ("\\.\\(?:djvu\\|eps\\)\\'" "open")
        ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
        ("\\.\\(?:xcf\\)\\'" "open")
        ("\\.csv\\'" "open")
        ("\\.tex\\'" "open")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
         "open")
        ("\\.\\(?:mp3\\|flac\\)\\'" "open")
        ("\\.html?\\'" "open")
        ("\\.md\\'" "open")))



(defvar dired-filelist-cmd
  '(("vlc" "-L")))

(add-hook 'term-mode-hook 'ash-term-hooks)


(global-prettify-symbols-mode 1)
(setq-default fill-column 110)

(setq recenter-positions '(top middle bottom))
;; delete the selection with a keypress
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

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " Guanghui - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; tramp, for sudo access
;; very slow!!!!
;; for profiling emacs --debug-init --timed-requires --profile
;; (require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
;; (setq tramp-default-method "ssh")

(set-default 'imenu-auto-rescan t)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)


;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
(require 'whitespace)


(setq whitespace-line-column fill-column) ;; limit line length
;;https://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))
(setq whitespace-style '(face tabs trailing tab-mark ))
;; (setq whitespace-style '(face lines-tail))
;; show tab;  use untabify to convert tab to whitespace
;; (setq spacemacs-show-trailing-whitespace nil)

(setq-default tab-width 4)
;; set-buffer-file-coding-system -> utf8 to convert dos to utf8
(setq inhibit-eol-conversion t)
(add-hook 'prog-mode-hook 'whitespace-mode)
;; (global-whitespace-mode +1)

(setq auto-mode-alist
      (append
       '(("\\.mak\\'" . makefile-mode))
       auto-mode-alist))

(setq large-file-warning-threshold 100000000)
;;http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/
;;need to install coreutils at first


;;add count for chinese, mainly used for writing chinese blog post
;; http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")


(require 'cc-mode)

;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior


(setq c-default-style "linux") ;; set style to "linux"
(setq c-basic-offset 4)
(c-set-offset 'substatement-open 0)

;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode t)

;; it's slow and seems not useful
;; (require 'semantic/bovine/c)
;; (require 'semantic/ia)
;; (defvar cocos2dx-dir "~/cocos2d-x")
;; (semantic-add-system-include cocos2dx-dir 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/cocos") 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/platform") 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/audio/include") 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/platform/mac") 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/extensions") 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include") 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/luajit") 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/freetype") 'c++-mode)
;; (semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/zlib") 'c++-mode)
;; ;; include path for OpenGL
;; (semantic-add-system-include "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers" 'c++-mode)
;; (add-to-list 'auto-mode-alist (cons cocos2dx-dir 'c++-mode))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_DLL" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("EXPORT_DLL" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("_USRDLL" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_TARGET_OS_MAC" . "1"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("_USRDLL" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_GUI_DLL" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_KEYBOARD_SUPPORT" . "1"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_DEPRECATED_ATTRIBUTE" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("USE_FILE32API" . "1"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_ENABLE_CHIPMUNK_INTEGRATION" . "1"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("COCOS2D_DEBUG" . "1"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat cocos2dx-dir "/cocos/platform/mac/CCPlatformDefine-mac.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat cocos2dx-dir "/cocos/platform/CCPlatformMacros.h"))

;; (set-default 'semantic-case-fold t)


(require 'font-lock)

(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
             'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
             'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
             'font-lock-comment-face)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


(add-hook 'c++-mode-hook
          '(lambda()
             (font-lock-add-keywords
              nil '(;; complete some fundamental keywords
                    ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
                    ;; add the new C++11 keywords
                    ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
                    ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
                    ;; PREPROCESSOR_CONSTANT
                    ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
                    ;; hexadecimal numbers
                    ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                    ;; integer/float/scientific numbers
                    ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
                    ;; user-types (customize!)
                    ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
                    ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
                    ))
             ) t)

;;add abbrev mode
;; stop aking whether to save newly added abbrev when quitting emacs

(define-abbrev-table 'global-abbrev-table '(

                                            ;; math/unicode symbols
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8luv" "♥")
                                            ("8smly" "☺")
                                            ("8en" "@~english")
                                            ("8zh" "@~chinese")
                                            ("8sp" "spacemacs")
                                            ;; email
                                            ("8me" "guanghui8827@gmail.com")

                                            ;; computing tech
                                            ("8wp" "Wikipedia")
                                            ("8ms" "Microsoft")
                                            ("8g" "Google")
                                            ("8it" "IntelliType")
                                            ("8msw" "Microsoft Windows")
                                            ("8win" "Windows")
                                            ("8ie" "Internet Explorer")
                                            ("8ahk" "AutoHotkey")
                                            ("82dx" "Cocos2D-X")

                                            ;; signature
                                            ("zl" "zilongshanren")
                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))
(setq save-abbrevs nil)

    ;; turn on abbrev mode globally
(setq-default abbrev-mode t)


(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))


;; reformat your json file, it requires python
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;; when save a buffer, the directory is not exsits, it will ask you to create the directory
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

(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("xml" "xsd" "rng" "xslt" "xsl") t) "\\'") 'nxml-mode))
(setq nxml-slash-auto-complete-flag t)


;; cleanup rencent files
(add-hook 'kill-emacs-hook #'(lambda () (progn (recentf-cleanup)
                                          (projectile-cleanup-known-projects))))

;; change evil initial mode state
(menu-bar-mode t)

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

 
(setq url-show-status nil)

(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'endless/org-ispell)

;;set region face for monokai theme
(set-face-attribute 'region nil :background "#696969")
