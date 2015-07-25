;;; nodejs-repl-eval.el --- Summary
;;; Commentary:
;;;
;;; Evaluation functions for the `nodejs-repl' package.  Written on a stormy
;;; night between days of node hacking.
;;;
;;; See https://gist.github.com/emallson/0eae865bc99fc9639fac
;;;
;;; Code:
(require 'js2-mode)
(require 'nodejs-repl)
(require 'dash)

;; (->> "var _ = require('lodash');
;;       _.chain(x)   // a comment here
;;       .filter('defined')
;;       .map(myfn);"
;;   (nodejs-repl--sanitize-code))

(defun nodejs-repl--sanitize-code (text)
  "Avoid conflicts with REPL special constructs: _ and .command"
  (->> text
    ;; obj EOL .fn() => obj. EOL fn() (while also removing "// comments")
    ;; If there is a chained call on a new line, move the dot to the previous line;
    ;; the repl executes lines eagerly and interprets " .something" as a REPL command
    (replace-regexp-in-string "\\(//.*\\)?\n\\(\\s-*\\)\\.\\(\\w+\\)" ".\n\\2\\3")
    ;; Replace _ with __ because underscore is a special thing in the REPL
    (replace-regexp-in-string "\\_<_\\." "__.")
    ;; Replace var _ = require ... with var __ = ...
    (replace-regexp-in-string "var\\s-+_\\s-+=" "var __ =")
    ))


(defun nodejs-repl-eval-region (start end)
  "Evaluate the region specified by `START' and `END'."
  (let ((proc (get-process nodejs-repl-process-name)))
    (comint-simple-send proc
                        (nodejs-repl--sanitize-code
                         (buffer-substring-no-properties start end)))))

(defun nodejs-repl-eval-node (node)
  "Evaluate `NODE', a `js2-mode' node."
  (let ((beg (js2-node-abs-pos node))
        (end (js2-node-abs-end node)))
    (nodejs-repl-eval-region beg end)))

(defun nodejs-repl--find-current-or-prev-node (pos &optional include-comments)
  "Locate the first node before `POS'.  Return a node or nil.

If `INCLUDE-COMMENTS' is set to t, then comments are considered
valid nodes.  This is stupid, don't do it."
  (let ((node (js2-node-at-point pos (not include-comments))))
    (if (or (null node)
            (js2-ast-root-p node))
        (unless (= 0 pos)
          (nodejs-repl--find-current-or-prev-node (1- pos) include-comments))
      node)))

(defun nodejs-repl-eval-function ()
  "Evaluate the current or previous function."
  (interactive)
  (let* ((fn-above-node (lambda (node)
                         (js2-mode-function-at-point (js2-node-abs-pos node))))
        (fn (funcall fn-above-node
             (nodejs-repl--find-current-or-prev-node
              (point) (lambda (node)
                        (not (null (funcall fn-above-node node))))))))
    (unless (null fn)
      (nodejs-repl-eval-node fn))))

(defun nodejs-repl-eval-first-stmt (pos)
  "Evaluate the first statement found from `POS' by `js2-mode'.

If this statement is a block statement, its first parent
statement is found.  This will be either a function declaration,
function call, or assignment statement."
  (let ((node (js2-mode-find-first-stmt (nodejs-repl--find-current-or-prev-node pos))))
    (cond
     ((js2-block-node-p node) (nodejs-repl-eval-node (js2-node-parent-stmt node)))
     ((not (null node)) (nodejs-repl-eval-node node)))))

(defun nodejs-repl-eval-dwim ()
  "Heuristic evaluation of JS code in a NodeJS repl.

Evaluates the region, if active, or the first statement found at
or prior to the point.

If the point is at the end of a line, evaluation is done from one
character prior.  In many cases, this will be a semicolon and will
change what is evaluated to the statement on the current line."
  (interactive)
  (cond
   ((use-region-p) (nodejs-repl-eval-region (region-beginning) (region-end)))
   ((= (line-end-position) (point)) (nodejs-repl-eval-first-stmt (1- (point))))
   (t (nodejs-repl-eval-first-stmt (point)))))

(defun nodejs-repl-eval-buffer (&optional buffer)
  "Evaluate the current buffer or the one given as `BUFFER'.

`BUFFER' should be a string or buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (nodejs-repl-eval-region (point-min) (point-max)))))

(provide 'nodejs-repl-eval)
