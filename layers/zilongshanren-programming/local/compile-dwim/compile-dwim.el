;;; compile-dwim.el --- Automatic generate compile-command

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 8 Dec 2007
;; Modified: 6 Jan 2019 by zilongshanren
;; Version: 0.01
;; Package-Version: 20090830.1840
;; Keywords: tools
;;
;; This file is part of PDE (Perl Development Environment).
;; But it is useful for generic programming.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Features:
;; 1. Smart compile and run command for associated mode.
;; 2. File timestamp check both compiling and runing
;; 3. Easy for customization

;;; Dependencies:
;;; no extra libraries is required

;;; Installation:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'compile-dwim)

;;; See also:
;; smart-compile.el by Seiji Zenitani <zenitani@mac.com>
;; smart-compile+.el by William XWL <william.xwl@gmail.com>

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'format-spec)
(require 'compile)

(defgroup compile-dwim nil
  "Automatic generate compile-command"
  :group 'tools
  :group 'pde)

(defcustom compile-dwim-check-tools t
  "Whether checking makefile or ant or else."
  :type 'boolean
  :group 'compile-dwim)

(defvar compile-dwim-cache nil
  "Last commands selected.")
(put 'compile-dwim-cache 'safe-local-variable 'listp)

(defvar compile-dwim-cache-type nil
  "Record which type of the compile-dwim-cache is valid.
Its value should be 'always or list like (filename run compile).")

;;;###autoload 
(defcustom compile-dwim-alist
  `((perl (or (name . "\\.pl$")
              (mode . cperl-mode))
          "%i -wc \"%f\"" "%i \"%f\"")
    (c    (or (name . "\\.c$")
              (mode . c-mode))
          ("gcc -o %n %f" "gcc -g -o %n %f") ("./%n" "cint %f") "%n")
    (c++  (or (name . "\\.cpp$")
              (mode . c++-mode))
          ("g++ -o %n %f" "g++ -g -o %n %f") "./%n" "%n")
    (java (or (name . "\\.java$")
              (mode . java-mode))
          "javac %f" "java %n" "%n.class")
    (python (or (name . "\\.py$")
                (mode . python-mode))
            "python %f" "python %f")
    (javascript (or (name . "\\.js$")
                    (mode . javascript-mode))
                "node  %f" "node  %f")
    (tex   (or (name . "\\.tex$")
               (name . "\\.ltx$")
               (mode . tex-mode)
               (mode . latex-mode))
           "latex %f" "latex %f" "%n.dvi")
    (texinfo (name . "\\.texi$")
             (makeinfo-buffer) (makeinfo-buffer) "%.info")
    (sh    (or (name . "\\.sh$")
               (mode . sh-mode))
           "%i ./%f" "%i ./%f")
    (f99   (name . "\\.f90$")
           "f90 %f -o %n" "./%n" "%n")
    (f77   (name . "\\.[Ff]$")
           "f77 %f -o %n" "./%n" "%n")
    (php   (or (name . "\\.php$")
               (mode . php-mode))
           "php %f" "php %f")
    (elisp (or (name . "\\.el$")
               (mode . emacs-lisp-mode)
               (mode . lisp-interaction-mode))
           (emacs-lisp-byte-compile) (emacs-lisp-byte-compile) "%fc"))
  "Settings for certain file type.
A list like ((TYPE CONDITION COMPILE-COMMAND RUN-COMMAND EXE-FILE) ...).
In commands, these format specification are available:

  %i  interpreter name
  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extention  ( netscape )
  %e  extention of file name       ( bin )

The interpreter is the program in the shebang line. If the
program is valid(test with `executable-find'), then use this program,
otherwise, use interpreter in `interpreter-mode-alist' according
to the major mode."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'compile-dwim)

(defvar compile-dwim-run-buffer nil
  "Internal variable used by `compile-dwim-run'.
`compile-dwim-prompt-run' can't remember which buffer last used.")

(defcustom compile-dwim-interpreter-alist
  '((perl . "perl"))
  "*Interpreter for file type.
The CAR is the type defined in `compile-dwim-alist', the CDR is
the associated interpreter name. Usually, you don't have to set
this, the default interpreter can be found in
`interpreter-mode-alist'. Unfortunately `cperl-mode' is not in
that alist."
  :type '(alist :key-type symbol :value-type string)
  :group 'compile-dwim)

(defsubst compile-dwim-conf (name conf)
  (nth (assoc-default name '((type . 0)
                             (condition . 1)
                             (compile . 2)
                             (run . 3)
                             (exe . 4))) conf))

(defun compile-dwim-interpreter (type)
  (let (interpreter)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "#!\\s-*\\(\\S+.*\\)")
        ;; remove file variables
        (setq interpreter (replace-regexp-in-string
                           "-\\*-.*" ""
                           (substring-no-properties (match-string 1))))
        (unless (and (> (length interpreter) 0)
                     (executable-find (car (split-string interpreter))))
          (setq interpreter nil)))
      (or interpreter
          (cdr (assq type compile-dwim-interpreter-alist))
          (car (rassq major-mode interpreter-mode-alist))
          ""))))

(defsubst compile-dwim-spec (type)
  (format-spec-make
   ?i (compile-dwim-interpreter type)
   ?F (buffer-file-name)
   ?f (file-name-nondirectory (buffer-file-name))
   ?n (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
   ?e (file-name-extension (buffer-file-name))))

(defun compile-dwim-match-1 (buf filter)
  (cond ((eq (car filter) 'name)
         (and (buffer-file-name buf)
              (string-match (cdr filter) (buffer-file-name buf))))
        ((eq (car filter) 'mode)
         (eq (cdr filter) (buffer-local-value 'major-mode buf)))
        (t (error "Unimplement filter: %s" filter))))

(defun compile-dwim-match (buf filters)
  (cond ((eq (car filters) 'or)
         (let (result)
           (setq filters (cdr filters))
           (while filters
             (if (compile-dwim-match buf (car filters))
                 (setq result t
                       filters nil)
               (setq filters (cdr filters))))
           result))
        ((eq (car filters) 'not)
         (not (compile-dwim-match buf (cadr filters))))
        (t (not (null (compile-dwim-match-1 buf filters))))))

(defsubst compile-dwim-make-local-vars ()
  (mapc (lambda (var)
          (or (local-variable-p var)
              (set (make-local-variable var) nil)))
        '(compile-dwim-cache compile-dwim-cache-type)))

(defun compile-dwim-calculate-command (type)
  (let ((alist compile-dwim-alist)
        match priority)
    (while alist
      (if (compile-dwim-match (current-buffer) (cadr (car alist)))
          (setq match (car alist)
                alist nil)
        (setq alist (cdr alist))))
    (when match
      ;; if the compile-command is setting by file variable or some
      ;; hook, the compile-dwim-cache should be empty and compile-command
      ;; should become local variable. so just return it
      (if (and (null compile-dwim-cache)
               (local-variable-p 'compile-command))
          (progn
            (setq compile-dwim-cache
                  `((compile . ,compile-command)
                    (run . ,compile-command)))
            (cons (car match) (list compile-command)))
        (let ((cmds (compile-dwim-conf type match))
              (spec (compile-dwim-spec (car match)))
              lisp-cmd)
          (setq cmds (delq nil (mapcar (lambda (cmd)
                                         (if (stringp cmd)
                                             (format-spec cmd spec)
                                           ;; if it is a symbol, make a funcall
                                           (setq lisp-cmd (if (listp cmd)
                                                              cmd (list cmd)))
                                           nil))
                                       ;; if it is a string or symbol, make a list
                                       (if (listp cmds)
                                           cmds (setq cmds (list cmds))))))
          (when (not lisp-cmd)
            ;; add makefile etc when compile
            (when (and (eq type 'compile) compile-dwim-check-tools)
              (cond ((or (file-readable-p "Makefile")
                         (file-readable-p "makefile"))
                     (push "make" cmds))
                    ((file-readable-p "build.xml")
                     (push "ant" cmds))))
            ;; if compile-dwim-cache-type is null, and compile-dwim-cache is set,
            ;; so it is set in file variable:
            (cond ((null compile-dwim-cache-type)
                   (if compile-dwim-cache
                       (setq compile-dwim-cache-type 'always)
                     (setq compile-dwim-cache-type (list buffer-file-name type))))
                  ;; if file name is changed, put the cache lower priority
                  ((listp compile-dwim-cache-type)
                   (if (string= (car compile-dwim-cache-type) buffer-file-name)
                       (if (memq type (cdr compile-dwim-cache-type))
                           nil
                         (setq priority 'lower)
                         (setq compile-dwim-cache-type
                               (list buffer-file-name (delete-dups (cons type (cdr compile-dwim-cache-type))))))
                     (setq priority 'lower)
                     (setq compile-dwim-cache-type (list buffer-file-name type)))))
            (let ((oldcmds (delq nil
                                 (mapcar (lambda (cmd)
                                           (if (eq (car cmd) type)
                                               (cdr cmd)))
                                         compile-dwim-cache))))
              (if (eq priority 'lower)
                  (setq cmds (delete-dups (nconc cmds oldcmds)))
                (setq cmds (delete-dups (nconc oldcmds cmds))))))
          (cons (car match) (or lisp-cmd cmds)))))))

;;;###autoload
(defun compile-dwim-compile (force &optional sentinel)
  (interactive "P")
  (if (not (buffer-file-name))
      (call-interactively 'compile)
    (compile-dwim-make-local-vars)
    (let ((cmds (compile-dwim-calculate-command 'compile))
          match exe spec cancel)
      (if (null cmds)
          (call-interactively 'compile)
        (setq match (assoc (car cmds) compile-dwim-alist))
        (when (and (not force)
                   (setq exe (compile-dwim-conf 'exe match)))
          (setq spec (compile-dwim-spec (car match))
                exe (format-spec exe spec))
          (when (and (file-exists-p exe)
                     (time-less-p (nth 5 (file-attributes (buffer-file-name)))
                                  (nth 5 (file-attributes exe))))
            (message "The exe file is newer! No need to compile!")
            (setq cancel t)))
        (when (not cancel)
          (setq cmds (cdr cmds))
          (if (null cmds)
              (message "No compile command found!")
            (if (stringp (car cmds))
                (progn
                  (setq compile-command (car cmds)
                        compile-history (nconc cmds compile-history))
                  (if sentinel
                      (add-hook 'compilation-finish-functions sentinel))
                  (call-interactively 'compile)
                  (add-to-list 'compile-dwim-cache
                               (cons 'compile compile-command)))
              (eval cmds)
              (if sentinel (funcall sentinel)))))))))

(defun compile-dwim-prompt-run (&rest ignore)
  ;; this function should call only once
  (remove-hook 'compilation-finish-functions 'compile-dwim-prompt-run)
  (when (yes-or-no-p "Compilation finished, run it now? ")
    (set-buffer compile-dwim-run-buffer)
    (compile-dwim-run)))

;;;###autoload
(defun compile-dwim-run ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'compile)
    (compile-dwim-make-local-vars)
    (let ((cmds (compile-dwim-calculate-command 'run))
          match exe spec cancel)
      (if (null cmds)
          (call-interactively 'compile)
        (setq match (assoc (car cmds) compile-dwim-alist))
        (when (setq exe (compile-dwim-conf 'exe match))
          (setq spec (compile-dwim-spec (car match))
                exe (format-spec exe spec))
          (if (not (file-exists-p exe))
              (progn
                (compile-dwim-compile t 'compile-dwim-prompt-run)
                (setq compile-dwim-run-buffer (current-buffer))
                (setq cancel t))
              (when (and (file-exists-p exe)
                         (time-less-p (nth 5 (file-attributes exe))
                                      (nth 5 (file-attributes (buffer-file-name)))))
                (setq cancel t)
                (when (yes-or-no-p "The exe file is expired, should we compile first? ")
                  (setq compile-dwim-run-buffer (current-buffer))
                  (compile-dwim-compile t 'compile-dwim-prompt-run)))))
        (when (not cancel)
          (setq cmds (cdr cmds))
          (if (null cmds)
              (message "No compile command found!")
            (if (stringp (car cmds))
                (progn
                  (setq compile-command (car cmds)
                        compile-history (nconc cmds compile-history))
                  (call-interactively 'compile)
                  (add-to-list 'compile-dwim-cache
                               (cons 'run compile-command)))
              (eval cmds))))))))

(provide 'compile-dwim)
;;; compile-dwim.el ends here
