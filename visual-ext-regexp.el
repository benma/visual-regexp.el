(require 'visual-regexp)

;;; variables

(defcustom vr/command-python (format "python %s" (expand-file-name "regexp.py" (file-name-directory load-file-name)))
  "External command used for the Python engine."
  :type 'string
  :group 'visual-regexp)

(defcustom vr/command-custom ""
  "Custom external command used when the engine is set to custom."
  :type 'string
  :group 'visual-regexp)

(defcustom vr/engine 'python
  "Which engine to use for searching/replacing. 
Use Emacs to use Emacs-style regular expressions.
Use Python to use Python's regular expressions (see vr/command-python).
Use Custom to use a custom external command (see vr/command-custom)."
  :type '(choice
	  (const :tag "Emacs" emacs) 
	  (const :tag "Python" python)
	  (const :tag "Custom" custom))
  :group 'visual-regexp)

(defcustom vr/default-regexp-modifiers '(:I nil :M t :S nil :U nil)
  "Modifiers that are applied by default. All modifiers are: '(I M S U).
See also: http://docs.python.org/library/re.html#re.I"
  ;;:type '(choice (const 10) (const 5))

  :type '(plist :key-type (choice
			   (const :tag "Enable the IGNORECASE modifier by default" :I) 
			   (const :tag "Enable the MULTILINE modifier by default (^ and $ match on every line)" :M)
			   (const :tag "Enable the DOTALL modifier by default (dot matches newline)" :S)
			   (const :tag "Enable the UNICODE modifier by default" :U))
		:value-type boolean)
  :group 'visual-regexp
  )

;;; private variables

(defvar vr--use-expression nil
  "Use expression instead of string in replacement.")

;; modifiers IMSU (see http://docs.python.org/library/re.html#re.I)
(defvar vr--regexp-modifiers '()
  "Modifiers in use.")

(define-key vr/minibuffer-regexp-keymap (kbd "C-c i") (lambda () (interactive) (vr--toggle-regexp-modifier :I)))
(define-key vr/minibuffer-regexp-keymap (kbd "C-c m") (lambda () (interactive) (vr--toggle-regexp-modifier :M)))
(define-key vr/minibuffer-regexp-keymap (kbd "C-c s") (lambda () (interactive) (vr--toggle-regexp-modifier :S)))
(define-key vr/minibuffer-regexp-keymap (kbd "C-c u") (lambda () (interactive) (vr--toggle-regexp-modifier :U)))

(define-key vr/minibuffer-replace-keymap (kbd "C-c C-c") (lambda () (interactive)
							   (when (equal vr--in-minibuffer 'vr--minibuffer-replace)
							     (setq vr--use-expression (not vr--use-expression))
							     (vr--update-minibuffer-prompt)
							     (vr--do-replace-feedback))))


;;; regexp modifiers

(add-hook 'vr/initialize-hook (lambda ()
				(setq vr--use-expression nil)
				(setq vr--regexp-modifiers (copy-sequence vr/default-regexp-modifiers))))

(defun vr--regexp-modifiers-enabled ()
  (eq vr/engine 'python))

(defun vr--toggle-regexp-modifier (modifier)
  "modifier should be one of :I, :M, :S, :U."
  (when (vr--regexp-modifiers-enabled)
    (plist-put vr--regexp-modifiers modifier 
	       (not (plist-get vr--regexp-modifiers modifier)))
    (vr--update-minibuffer-prompt)
    (vr--feedback)))

(defun vr--get-regexp-modifiers-prefix ()
  "Construct (?imsu) prefix based on selected modifiers."
  (if (vr--regexp-modifiers-enabled)
      (let ((s (mapconcat 'identity 
			  (delq nil (mapcar (lambda (m)
					      (when (plist-get vr--regexp-modifiers m)
						(cond ((equal m :I) "i")
						      ((equal m :M) "m")
						      ((equal m :S) "s")
						      ((equal m :U) "u")
						      (t nil))))
					    (list :I :M :S :U)))
			  "")))
	(if (string= "" s) "" (format "(?%s)" s)))
    ""))


(defadvice vr--get-regexp-string (around get-regexp-string-prefix-modifiers () activate)
  ad-do-it
  (setq ad-return-value
	(concat (vr--get-regexp-modifiers-prefix) 
		ad-return-value)))

;;; shell command / parsing functions

(defun vr--get-command ()
  (cond
   ((eq vr/engine 'python) vr/command-python)
   ((eq vr/engine 'custom) vr/command-custom)))

(defun vr--command (command)
  (let ((stdout-buffer (generate-new-buffer (generate-new-buffer-name " *pyregex stdout*")))
	output
	exit-code)
    (with-current-buffer vr--target-buffer
      (setq exit-code (call-process-region
		       vr--target-buffer-start
		       vr--target-buffer-end
		       shell-file-name
		       nil ;; don't delete region
		       stdout-buffer
		       nil ;; don't redisplay buffer
		       shell-command-switch
		       command)))
    (with-current-buffer stdout-buffer
      (setq output (buffer-string))
      (kill-buffer))
    (list output exit-code)))

(defun vr--run-command (args success)
  (multiple-value-bind (output exit-code) (vr--command args)
    (cond ((equal exit-code 0) 
	   (funcall success output))
	  ((equal exit-code 1)
	   (message "script failed:%s\n" output))
	  (t (error (format "External command failed with exit code %s" exit-code))))))

(defun vr--unescape (s) ;; todo: should not be needed here
  "Replacement strings returned by external script have escaped newlines and backslashes (so that there can be one replacement per line). Unescape to get back original.
Escaped newlines are only unescaped if newline is not nil."
  (setq s (replace-regexp-in-string (regexp-quote "\\n") (regexp-quote "\n") s))
  (replace-regexp-in-string (regexp-quote "\\\\") (regexp-quote "\\") s))

(defun vr--not-last-line () 
  "Output of external script ends in one line of message and one empty line.
Return t if current line is not the line with the message."
  (save-excursion (= 0 (forward-line 2))))

(defun vr--current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun vr--parse-matches (s callback)
  "Parse string s with positions of matches and groups as returned by external script. For each position, callback is called with arguments (i j begin end),
i being the match and j the group index and begin/end being the span of the match.
The message line is returned.
"
  (let (message-line) ;; store message line (last non-empty line of output)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (let ((offset vr--target-buffer-start))
	(loop while (and (vr--not-last-line) (/= (line-beginning-position) (line-end-position))) ;; loop until empty line is reached
	      for i from 0 do
	      (loop while (re-search-forward "\\([0-9]+\\) \\([0-9]+\\)" (line-end-position) t) ;; loop integer pairs in line
		    for j from 0 do
		    (let ((begin (+ offset (string-to-number (match-string 1))))
			  (end (+ offset (string-to-number (match-string 2)))))
		      (funcall callback i j begin end)))
	      (forward-line 1)))
      (setq message-line (vr--unescape (vr--current-line))))
    message-line))

(defun vr--parse-replace (s)
  "Parse string s with positions of matches and replacements as returned by external script.
Returns a list, in reverse order, of (replacement begin end i) (i = index of match = index of corresponding overlay)
and the message line."
(let ((replacements (list)) ;; store replacements (lines of output) in list
	message-line) ;; store message line (last non-empty line of output)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (loop while (and (vr--not-last-line) (/= (line-beginning-position) (line-end-position))) ;; loop until empty line is reached
	    for i from 0 do 
	    (re-search-forward "\\([0-9]+\\) \\([0-9]+\\) " (line-end-position) t)
	    (let ((replacement (buffer-substring-no-properties (point) (line-end-position)))
		  (begin (+ vr--target-buffer-start (string-to-number (match-string 1))))
		  (end (+ vr--target-buffer-start (string-to-number (match-string 2)))))
	      (setq replacements (cons (list (vr--unescape replacement) begin end i) replacements)))
	    (forward-line 1))
      (setq message-line (vr--unescape (vr--current-line))))
    (list replacements message-line)))

:;; prompt

(defadvice vr--set-minibuffer-prompt-regexp (around prompt-regexp activate)
  (setq ad-return-value
	(if (vr--regexp-modifiers-enabled)
	    (format "Regexp: %s" (vr--get-regexp-modifiers-prefix))
	  "Regexp: ")))

(defadvice vr--set-minibuffer-prompt-replace (around prompt-replace activate)
  (setq ad-return-value
	(concat "Replace"
		(let ((flag-infos (mapconcat 'identity 
					     (delq nil (list (when vr--use-expression "using expression")
							     (when vr--replace-preview "preview"))) 
					     ", ")))
		  (when (not (string= "" flag-infos ))
		    (format " (%s)" flag-infos)))
		(format " (%s)" (vr--get-regexp-string))
		": ")))

;; feedback / replace functions

(defadvice vr--feedback-function (around feedback-around (forward feedback-limit callback) activate)
  "Feedback function for search using an external command."
  (if (eq vr/engine 'emacs)
      ad-do-it
    (setq ad-return-value
	  (vr--run-command 
	   (format "%s matches --regexp %s %s %s"
		   (vr--get-command)
		   (shell-quote-argument regexp-string)
		   (when feedback-limit (format "--feedback-limit %s" feedback-limit))
		   (if forward "" "--backwards")
		   )
	   (lambda (output)
	     (vr--parse-matches
	      output 
	      callback))))))

(defadvice vr--get-replacements (around get-replacements-around (feedback feedback-limit) activate)
  "Get replacements using an external command."
  (if (eq vr/engine 'emacs)
      ad-do-it
    (setq ad-return-value
	(vr--run-command 
	 (format "%s replace %s %s %s --regexp %s --replace %s"
		 (vr--get-command)
		 (if feedback "--feedback" "")
		 (if feedback-limit
		     (format "--feedback-limit %s" feedback-limit)
		   "")
		 (if vr--use-expression "--eval" "")
		 (shell-quote-argument (vr--get-regexp-string))
		 (shell-quote-argument replace-string))
	 'vr--parse-replace))))

(provide 'visual-ext-regexp)
