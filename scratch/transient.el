(defun test-function (&optional args)
  (interactive
   (list (transient-args 'test-transient)))
  (message "args: %s" args))

(transient-define-prefix test-transient ()
  "Test Transient Title"
  ;; :transient-suffix     'transient--do-stay
  :transient-suffix     'transient--do-call
  ["Arguments"
   ("-s" "Switch" "--switch")
   ("-a" "Another switch" "--another")]
  ["Actions"
   ("d" "Action d" test-function)])

(defun foo (&optional args)
  (interactive (list (transient-args 'outline-navigate)))
  (message "args %S" args))

(define-transient-command outline-navigate ()
  :transient-suffix     'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["Arguments"
   ("-t" "test" "--test")]
  [("p" "previous visible heading" outline-previous-visible-heading)
   ("n" "next visible heading" outline-next-visible-heading)
   ("f" "foo" foo)])



(define-transient-command hrm-help-transient ()
  "Help commands that I use. A subset of C-h with others thrown in."
  ["Help Commands"
   ["Mode & Bindings"
    ("m" "Mode" describe-mode)
    ("b" "Major Bindings" which-key-show-full-major-mode)
    ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
    ;; ("d" "Descbinds" counsel-descbinds)
    ("t" "Top Bindings  " which-key-show-top-level)
    ]
   ["Describe"
    ;; ("C" "Command" helpful-command)
    ;; ("f" "Function" helpful-callable)
    ;; ("v" "Variable" helpful-variable)
    ;; ("k" "Key" helpful-key)
    ;; ("c" "Key Briefly" describe-key-briefly)
    ]
   ["Info on"
    ("C-c" "Emacs Command" Info-goto-emacs-command-node)
    ;; ("C-f" "Function" counsel-info-lookup-symbol) ; s for symbol?
    ;; ("C-v" "Variable" counsel-info-lookup-symbol) ; . for symbol?
    ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
    ]
   ["Goto Source"
    ("L" "Library" find-library-other-frame)
    ("F" "Function" find-function-other-frame)
    ("V" "Variable" find-variable-other-frame)
    ("K" "Key" find-function-on-key-other-frame)
    ]
   ]
  [
   ["Internals"
    ("I" "Input Method" describe-input-method)
    ("G" "Language Env" describe-language-environment)
    ("S" "Syntax" describe-syntax)
    ("O" "Coding System" describe-coding-system)
    ("C-o" "Coding Brief" describe-current-coding-system-briefly)
    ("T" "Display Table" describe-current-display-table)
    ("e" "Echo Messages" view-echo-area-messages)
    ("l" "Lossage" view-lossage)
    ]
   ["Describe"
    ;; ("s" "Symbol" helpful-symbol)
    ;; ("." "At Point   " helpful-at-point)
    ;; ("C-f" "Face" counsel-describe-face)
    ("w" "Where Is" where-is)
    ("=" "Position" what-cursor-position)
    ]
   ["Info Manuals"
    ("C-i" "Info" info)
    ("C-4" "Other Window " info-other-window)
    ("C-e" "Emacs" info-emacs-manual)
    ;; ("C-l" "Elisp" info-elisp-manual)
    ]
   ["External"
    ;; ("W" "Dictionary" lookup-word-at-point)
    ("Z" "Zeal" zeal-at-point)
    ]
   ]
  )


(define-transient-command hrm-customize-transient ()
  "Customize Emacs settings"
  ["Customize Emacs settings"
   ["Global"
    ("C" "Customize" customize)
    ("B" "Browse" customize-browse)
    ("A" "Apropos" customize-apropos)
    ]
   ["Basic"
    ("g" "Group" customize-group)
    ("o" "Option" customize-option)
    ("v" "Option" customize-option)
    ("f" "Face" customize-face)
    ]
   ["Other Window"
    ("4g" "Group" customize-group-other-window)
    ("4o" "Option" customize-option-other-window)
    ("4v" "Option" customize-option-other-window)
    ("4f" "Face" customize-face-other-window)
    ]
   ]
  ["Advanced"
   ["Apropos"
    ("aa" "Everything" customize-apropos)
    ("aa" "Everything 2" customize-apropos)
    ("ag" "Groups" customize-apropos-groups)
    ("ao" "Options" customize-apropos-options)
    ("af" "Faces" customize-apropos-faces)
    ]
   ["Search"
    ("n" "Since Version" customize-changed-options)
    ("u" "Unsaved" customize-unsaved)
    ("s" "Saved" customize-saved)
    ("r" "Rogue" customize-rogue) ; set outside custom
    ]
   ["Themes"
    ("t" "Theme" customize-themes)
    ("T" "Create Theme" customize-create-theme)
    ]
   ]
  )




(defun test-function (&optional args)
  (interactive
   (list (transient-args 'test-transient)))
  (message "args %s" args))

(define-infix-argument test-transient:--message ()
  :description "Message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message=")

(define-transient-command test-transient ()
  "Test Transient Title"
  ["Arguments"
   ("-s" "Switch" "--switch")
   ("-a" "Another switch" "--another")
   (test-transient:--message)]
  ["Actions"
   ("d" "Action d" test-function)])

(test-transient)


(define-infix-argument s7bi/s7pd-file-arg ()
  :description "s7pd file"
  :class 'transient-option
  :shortarg "-f"
  :argument "file="
  :reader (lambda (_prompt _initial-input _history)
            (read-file-name "s7pd file:")))

(defun sharper--get-argument (marker transient-params)
  "Extract from TRANSIENT-PARAMS the argument with  MARKER."
  (cl-some
   (lambda (an-arg) (when (string-prefix-p marker an-arg)
                      (replace-regexp-in-string marker
                                                ""
                                                an-arg)))
   transient-params))

(defun sharper--get-switch (marker transient-params)
  "Extract from TRANSIENT-PARAMS the argument with  MARKER."
  (cl-some
   (lambda (an-arg) (message "an arg %S" an-arg) (string-prefix-p marker an-arg))
   transient-params))

(defun s7bi/s7pd-command (&optional args)
  (interactive
   (list (transient-args 's7bi/s7pd-transient)))
  (message "args: %s" args)
  (message "file? %s" (sharper--get-argument "file=" args))
  (message "switch? %s" (sharper--get-argument "--switch" args)))

(define-transient-command s7bi/s7pd-transient ()
  "Test Transient Title"
  ["Arguments"
   ("-s" "" "--switch")q
   ("-a" "Another switch" "--another")
   ("y" "Yet Another switch" 'yet-another)
   (s7bi/s7pd-file-arg)]
  ["Actions"
   ("r" "repl" s7bi/s7pd-command)])



(s7bi/s7pd-transient)
