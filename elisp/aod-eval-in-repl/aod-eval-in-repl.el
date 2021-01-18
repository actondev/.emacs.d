;;; aod-eval-in-repl.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

;; Copyleft (C) 2020- actondev

;; Author: actondev
;; Keywords: tools, literate programming
;; URL: https://github.com/actondev/tbd

;; This file is not part of GNU Emacs.

;;; Code:

;; running tests:
;; (ert "aod.eir/*")
(defun aod.eir/src-block-info-light ()
  "NOTE: this seems to have been fixed in org 9.3
Returns the src-block-info without evaluating anything.
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
  (or (org-babel-read (aod.eir/-sesion-name-from-opts opts))
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

(cl-defgeneric aod.eir/validate-eval-p (lang session string opts)
  (let ((trunc-length 50)
	(case-fold-search nil))
    ;; I'm usually naming session like STAGING, PROD, DEV or something
    ;; so, if there are 3 capitaler letter, prompt for validation
    ;; TODO make this a defcustom
    (if (string-match-p "[A-Z]\\{3,\\}" session)
	(y-or-n-p (format "Confirm: eval in %s: %s" session (truncate-string-to-width string trunc-length 0 nil "..")))
      t)))

(cl-defgeneric aod.eir/eval (lang session string opts)
  "Default implementation for eval. Could be extended with cl-defmethod for a specific lang.
TODO 
- should this one start the repl if not present?
- should session be passed to make usable/friendly beyond org-mode contexts?"
  (save-current-buffer
    (set-buffer session)
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

(defun aod.eir/org-src-point ()
  (let ((boundaries (org-src--contents-area (org-element-at-point))))
    (- (point) (car boundaries))))

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

(defvar aod.eir/opts-multi-keys '(:replace :init)
  "Keys that support multiple space-separated statements, like the `:var'
ie `:var a=1 b=2` gets parsed to give ((:var . \"a=1\") (:var \"b=2\")
Without this parsing it would give ((:var . \"a=1 b=2\"")

(defun aod.eir/block-contents-replaced (name &optional opts)
  (org-save-outline-visibility nil ;; use markers?
    (save-excursion
      (goto-char (org-babel-find-named-block name))
      (let ((src (org-element-property :value (org-element-at-point)))
	    (opts (or opts
		      (aod.eir/parse-opts (nth 2 (org-babel-get-src-block-info 'light))))))
	(aod.eir/process-string src opts)))))

(defun aod.eir/parse-opts (opts)
  "Parses opts for `aod.eir/opts-multi-keys'"
  (let (results)
    (mapc (lambda (pair)
	    (let ((key (car pair)))
	      (if (member key aod.eir/opts-multi-keys)
		  (progn
		    ;; (mapcar (lambda (v) (push (cons key v) results))
		    ;; 	    (split-string (cdr pair) " "))
		    ;; TODO remove the org-* functions from here
		    (mapcar (lambda (v)
			      (push (cons key (org-trim v)) results))
			    ;; 32 is space
			    (org-babel-balanced-split (cdr pair) 32)
			    ;;(split-string (cdr pair) "(?![^(]*\)) ")
			    )
		    )
		(push pair results))))
	  opts)
    (nreverse results)))

(ert-deftest aod.eir/parse-opts ()
  (let ((aod.eir/opts-multi-keys '(:foo :bar))
	(opts '((:meh . 1) (:foo . "a=1 b=2") (:bar . "c=3 d=4"))))
    (should (equal (aod.eir/parse-opts opts) '((:meh . 1) (:foo . "a=1") (:foo . "b=2") (:bar . "c=3") (:bar . "d=4")))))
  
  )

(ert-deftest aod.eir/parse-and-get-opts ()
  (let ((aod.eir/opts-multi-keys '(:replace))
	(opts '((:results . "replace") (:exports . "code") (:replace . "(\"aa\" \"hello\") (\"bb\" \"there\")") (:session . "*demo-replace*") (:tangle . "no") (:hlines . "no") (:noweb . "no") (:cache . "no"))))
    (should (equal (aod.eir/get-opts (aod.eir/parse-opts opts) :replace) '("(\"aa\" \"hello\")" "(\"bb\" \"there\")")))))

(defun aod.eir/get-opts (opts key)
  "PARAMS is a quasi-alist of header args, which may contain
multiple entries for eg the key `:var'.  This function returns a
list of the cdr of all the `:var' entries."
  (mapcar #'cdr
	  (cl-remove-if-not (lambda (x) (eq (car x) key)) opts)))

(defun aod.eir/parse-replacement (assignment)
  "Modified version of org-babel-ref-parse
Parse an x=y form in a header argument.
x cannot be a string (with quotes around it), so
if it has to be a string with spaces it can be done like
(concat \"foo bar\")=1 or (identity \"foo bar\")=1

If the right hand side of the assignment has a literal value
return that value, otherwise interpret it as a reference to an
external resource and find its value using `org-babel-ref-resolve'.

Return a list with two elements: the regex string to replace, and a
FUNCTION that when called returns the Emacs Lisp representation of the value of the value to replace with.
This is to avoid running the evaluation if the regex isn't found!"
  (if (string-match "\\(.+?\\)=" assignment)
      (let ((var (org-trim (match-string 1 assignment)))
	    (ref (org-trim (substring assignment (match-end 0)))))
	(cons (org-babel-read var)
	      (lambda ()
		(let ((out (org-babel-read ref)))
		  (if (equal out ref)
		      (cond ((and (not (string-prefix-p "(" ref))
				  (string-suffix-p ")" ref))
			     ;; it's a noweb call
			     (org-babel-ref-resolve ref))
			    ((org-babel-find-named-block ref)
			     (aod.eir/block-processed-contents ref))
			    (t (progn
				 ;; org-babel-read will not resolve variables
				 ;; but I want here to use :replace psql=var-holding-the-sql-conn-stirng
				 (eval (read ref)))))
		    out)))))
    ;; if no = , then return the symbol's value
    ;; buut remember, we actually return the string that we replace and it's replacement function
    (cons assignment
	  (lambda ()
	    (symbol-value (intern-soft assignment))))))

(defun aod.eir/process-string (string opts)
  "It provides a way to process the string before it's sent to
the repl. For example
#+begin_src sh :replace aa=1 bb=2
echo aa is not bb
#+end_src

Will send \"echo 1 is not 2\" to the repl"
  (let ((replaces (mapcar
		   (lambda (x)
		     (aod.eir/parse-replacement x))
		   (aod.eir/get-opts opts :replace))))
    (mapc (lambda (replace)
	    (let ((what (car replace))
		  ;; the with might be a noweb ref call
		  ;; in which case, org-babel-ref-resolve must be called
		  (with-fn (cdr replace))
		  (case-fold-search nil))
	      (when (string-match what string)
		(let ((with (funcall with-fn)))
		  (setq string (replace-regexp-in-string what (format "%s" with) string 'fixed 'literal))))))
	  replaces)
    string))

(ert-deftest aod.eir/process-string ()
  (let ((string "echo aa is bb")
	(opts '((:replace . "(\"aa\" \"three\")") (:replace . "(\"bb\" (+ 1 2))"))))
    (should (equal (aod.eir/process-string 'default-lang string opts) "echo three is 3"))))

(defun aod.eir/block-processed-contents (name)
  (org-save-outline-visibility nil ;; use markers?
    (save-excursion
      (goto-char (org-babel-find-named-block name))
      (let* ((el (org-element-at-point))
	     (src (org-element-property :value el))
	     (src-block-info (org-babel-get-src-block-info 'light el))
	     (opts (aod.eir/parse-opts (nth 2 src-block-info))))
	(aod.eir/process-string src opts)))))

(defun aod.eir/init-body (session opts)
  (let ((inits (aod.eir/get-opts opts :init))
	(body ""))
    (mapc (lambda (init-assignment)
	    (when (string-match "\\(.+?\\)=" init-assignment)
	      (let ((var (org-trim (match-string 1 init-assignment)))
		    (ref (org-trim (substring init-assignment (match-end 0)))))
		(when (string-equal session var)
		  (setq body (aod.eir/block-processed-contents ref))))))
	  inits)
    body))

(defun aod.eir/opts (&optional datum)
  (let ((src-block-info (org-babel-get-src-block-info 'light datum)))
    (aod.eir/parse-opts (nth 2 src-block-info))))

(defun aod.eir/-org-region-to-eval (lang opts)
  (if (region-active-p)
      (list (mark) (point))
    (aod.eir/-region-with-trimmed-whitespace
     (aod.eir/-constraint-region-to-element
      (aod.eir/get-region-to-eval lang opts)
      (org-element-at-point)))))

(defun aod.eir/eval-org-src ()
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info 'light))
	 (opts (aod.eir/parse-opts (nth 2 src-block-info)))
	 (lang (intern-soft (nth 0 src-block-info)))
	 (session (aod.eir/session-name lang opts)))
    (unless (aod.eir/-session-exists-p session)
      (let* (;;using org-babel-read and not eval cause
	     ;; "../" and (read-directory-name "dir ")
	     ;; both appear as strings. ie "../" doesn't seem
	     ;; to have exrta quotations anywhere.
	     ;; org-babel-read evals only if something starts with ( etc
	     (dir (org-babel-read (cdr (assq :dir opts))))
	     (default-directory
	       (or (and dir (file-name-as-directory (expand-file-name
						     dir)))
		   default-directory)))
	(message "starting repl %S at dir %S " lang default-directory)
	(save-selected-window (aod.eir/start-repl lang session opts))
	(aod.eir/eval lang session
		      (aod.eir/init-body session opts)
		      opts)))
    (let* ((region (aod.eir/-org-region-to-eval lang opts))
	   (string (aod.eir/process-string
		    (apply #'buffer-substring-no-properties region)
		    opts)))
      ;; TODO is this ok here?
      ;; also, make a param for the delay value
      (when (require 'nav-flash nil 'noerror)
	(let ((nav-flash-delay 0.1))
	  (apply #'nav-flash-show region)))
      (when (aod.eir/validate-eval-p lang session string opts)
	(aod.eir/eval lang session string opts)))))

(defun aod.eir/-remove-surrounding-stars (string)
  "Sometimes it's 'needed' (more like advised) to pass a session name with stars - eg calling (shell \"*shell-session*\") -, but other times the stars are added by them. eg from term, python etc"
  (replace-regexp-in-string "^[*]\\(.+\\)[*]$" "\\1" string))

(cl-defgeneric aod.eir/eof (lang string)
  (error "unknown way to send EOF string"))

(require 'aod-eval-in-repl-shell)
(require 'aod-eval-in-repl-python)
(require 'aod-eval-in-repl-sql)
(require 'aod-eval-in-repl-js)
(provide 'aod-eval-in-repl)
