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


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " Guanghui - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))



;; (require 'semantic/bovine/c)
;; (require 'semantic/ia)
;; (eval-after-load 'semantic-mode
;;   '(progn
;;      (defvar cocos2dx-dir "~/cocos2d-x")
;;      (semantic-add-system-include cocos2dx-dir 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/cocos") 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/platform") 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/audio/include") 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/platform/mac") 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/extensions") 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include") 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/luajit") 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/freetype") 'c++-mode)
;;      (semantic-add-system-include (concat cocos2dx-dir "/external/mac/x86-64/include/zlib") 'c++-mode)
;;      ;; include path for OpenGL
;;      (semantic-add-system-include "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers" 'c++-mode)
;;      (add-to-list 'auto-mode-alist (cons cocos2dx-dir 'c++-mode))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_DLL" . ""))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("EXPORT_DLL" . ""))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("_USRDLL" . ""))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_TARGET_OS_MAC" . "1"))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("_USRDLL" . ""))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_GUI_DLL" . ""))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_KEYBOARD_SUPPORT" . "1"))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_DEPRECATED_ATTRIBUTE" . ""))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("USE_FILE32API" . "1"))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CC_ENABLE_CHIPMUNK_INTEGRATION" . "1"))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("COCOS2D_DEBUG" . "1"))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat cocos2dx-dir "/cocos/platform/mac/CCPlatformDefine-mac.h"))
;;      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat cocos2dx-dir "/cocos/platform/CCPlatformMacros.h"))

;;      (set-default 'semantic-case-fold t)
;;      ))


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
                                            ("8zl" "zilongshanren")
                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))
