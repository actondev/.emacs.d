;;; aod-eval-in-repl.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

;; Copyleft (C) 2020- actondev

;; Author: actondev
;; Keywords: tools, literate programming
;; URL: https://github.com/actondev/tbd

;; This file is not part of GNU Emacs.

;;; Code:
(defun aod.eir/src-block-info-light ()
  "Returns the src-block-info without evaluating anything.
   While passing 'light to org-babel-get-src-block-info makes the
   :var definitions not evaluate any lisp expressions, other
   things (like :dir for example) get evaluated.

   For example, the following org src header would cause the
   elisp to be evaluated everytime upon calling
   org-babel-get-src-block-info

   :dir (read-directory-name \"dir name:\")"
  (cl-flet ((read-advice (read-orig in &rest _)
			 (funcall read-orig in 'inhibit-lisp-eval)))
    (advice-add 'org-babel-read :around #'read-advice)
    (let ((info (org-babel-get-src-block-info 'light)))
      (advice-remove 'org-babel-read #'read-advice)
      info)))

(defun aod.eir/-sesion-name-from-opts (opts)
  "Returns the session name from the org-mode src block info.
org-mode has a 'none' session if nothing is explicitly set. In that
case this function returns nil"
  (let* ((session (cdr (assq :session opts))))
    (if (string= session "none")
	nil
      session)))

(defvar aod.eir/default-session-alist
  '((python . "*Python*")
    (sh . "*shell*"))
  "The default session name per language.
TODO should this be a defcustom ??")

(defun aod.eir/session-name (lang opts)
  "Returns either the default session name, or the one from opts under the :session key"
  (or (aod.eir/-sesion-name-from-opts opts)
      (cdr (assq lang aod.eir/default-session-alist))
      (error "Could not find neither passed neither default session for lang %S" lang)))

(defun aod.eir/-session-exists-p (session)
  ;; (and (member session
  ;; 	       (mapcar #'buffer-name (buffer-list)))
  ;;      t)
  ;; NOTE from which version is this available?
  (seq-contains-p (mapcar #'buffer-name (buffer-list))
		  session))

(cl-defgeneric aod.eir/start-repl (lang session &optional opts)
  "Starts a repl for given lang. Lang should be a symbol, eg 'shell.
opts are in the format of `(nth 2 (org-babel-get-src-block-info))` output"
  (error "Don't know how to start a repl for lang %S" lang))

(cl-defgeneric aod.eir/eval (lang string opts)
  "Default implementation for eval. Could be extended with cl-defmethod for a specific lang.
TODO 
- should this one start the repl if not present?
- should session be passed to make usable/friendly beyond org-mode contexts?"
  (save-current-buffer
    (set-buffer (aod.eir/session-name lang opts))
    (aod.eir/send-string string)
    (aod.eir/send-input)))

(cl-defgeneric aod.eir/send-string (string)
  "Default implementation to send a string. Other logic can be implemented
with defmethod and using the &context"
  (goto-char (point-max))
  (insert string))

(cl-defgeneric aod.eir/send-input ()
  (comint-send-input))

(cl-defgeneric aod.eir/get-region-to-eval (lang &optional opts)
  "By default eval current line. Other implementations (eg sql)
get the current paragraph."
  (save-mark-and-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (list (mark) (point))))

(defun aod.eir/-constraint-region-to-element (region &optional element)
  (let* ((element (or element (org-element-at-point)))
	 (boundaries (org-src--contents-area element)))
    (list (max (car region) (car boundaries))
	  (min (cadr region) (cadr boundaries)))))

(defun aod.eir/forward-whitespace ()
  (interactive)
  (re-search-forward "[ \t\r\n\v\f]*"))

(defun aod.eir/backward-whitespace ()
  (interactive)
  (re-search-backward "[ \t\r\n\v\f]*"))

(defun aod.eir/-region-with-trimmed-whitespace (region)
  (save-mark-and-excursion
    (goto-char (car region))
    (aod.eir/forward-whitespace)
    (set-mark (point))
    (goto-char (cadr region))
    (aod.eir/backward-whitespace)
    (list (mark) (point))))

(defun aod.eir/-parse-multiple (header-arguments key)
  "Copied definition of org-babel-parse-multiple-vars to work more
cases than only :var.
Expand multiple variable assignments behind a single `key` keyword.

This allows expression of multiple variables with one `key` as
shown below.

#+PROPERTY: `key` foo=1, bar=2"
  (let (results)
    (mapc (lambda (pair)
	    (if (eq (car pair) key)
		(mapcar (lambda (v) (push (cons key (org-trim v)) results))
			(org-babel-join-splits-near-ch
			 61 (org-babel-balanced-split (cdr pair) 32)))
	      (push pair results)))
	  header-arguments)
    (nreverse results)))

(defun aod.eir/-get-multiple (params key)
  "Copied from
Return the babel variable assignments in PARAMS.

PARAMS is a quasi-alist of header args, which may contain
multiple entries for the key `:var'.  This function returns a
list of the cdr of all the `:var' entries."
  (mapcar #'cdr
	  (cl-remove-if-not (lambda (x) (eq (car x) key)) params)))

(cl-defgeneric aod.eir/process-string (lang string opts)
  "TODO should it generic?
It provides a way to process the string before it's sent to
the repl. For example
#+begin_src sh :replace (\"aa\" 1) (\"bb\" 2)
echo aa is not bb
#+end_src

Will send \"echo 1 is not 2\" to the repl"
  (let ((replaces (mapcar
		   (lambda (x) (car (read-from-string x)))
		   (aod.eir/-get-multiple
		    (aod.eir/-parse-multiple opts
					     :replace)
		    :replace))))
    (mapc (lambda (replace)
	    (let ((what (car replace))
		  (with (message "%s" (eval (cadr replace)))))
	      (setq string (replace-regexp-in-string what with string))))
	  replaces)
    string))

(defun aod.eir/eval-org-src ()
  (interactive)
  (let* ((src-block-info (aod.eir/src-block-info-light))
	 (opts (nth 2 src-block-info))
	 (lang (intern-soft (nth 0 src-block-info)))
	 (dir (cdr (assq :dir opts)))
	 (session (aod.eir/session-name lang opts)))
    (unless (aod.eir/-session-exists-p session)
      (message "starting repl %S" lang)
      (let ((default-directory
	      (or (and dir (file-name-as-directory (expand-file-name dir)))
		  default-directory)))
	(save-selected-window (aod.eir/start-repl lang session opts))))
    (let* ((region (aod.eir/-region-with-trimmed-whitespace
		    (aod.eir/-constraint-region-to-element
		     (aod.eir/get-region-to-eval lang opts)
		     (org-element-at-point))))
	   (string (aod.eir/process-string
		    lang
		    (apply #'buffer-substring-no-properties region)
		    opts)))
      ;; TODO is this ok here?
      ;; also, make a param for the delay value
      (when (require 'nav-flash nil 'noerror)
	(let ((nav-flash-delay 0.1))
	  (apply #'nav-flash-show region)))
      (aod.eir/eval lang string opts))))

(defun aod.eir/-remove-surrounding-stars (string)
  "Sometimes it's 'needed' (more like advised) to pass a session name with stars - eg calling (shell \"*shell-session*\") -, but other times the stars are added by them. eg from term, python etc"
  (replace-regexp-in-string "^[*]\\(.+\\)[*]$" "\\1" string))

(require 'aod-eval-in-repl-shell)
(require 'aod-eval-in-repl-python)
(require 'aod-eval-in-repl-sql)
(provide 'aod-eval-in-repl)
