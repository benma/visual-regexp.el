;;; visual-regexp.el --- A Python regexp/replace command for Emacs with interactive visual feedback

;; Copyright (C) 2012 Marko Bencun

;; Author : Marko Bencun <mbencun@gmail.com>
;; URL : https://github.com/benma/visual-regexp.el/
;; Version : 0.3
;; Keywords : regexp, replace, python, visual, feedback

;; This file is part of visual-regexp.

;; visual-regexp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; visual-regexp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with visual-regexp.  If not, see <http://www.gnu.org/licenses/>.

;;; WHAT'S NEW
;; 0.1: initial release

;;; INTRODUCTION
;;
;; What's This?
;;
;; It is a command for emacs which enables you to use Python regular expressions and either a Python string or a Python expression for doing replacements.
;; While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches.
;; While constructing the replacement in the minibuffer, you get live visual feedback for the replacements.

;; Where does visual-regexp come from?
;;
;; I was not happy with the way I used emacs' replace-regexp before. Constructing the regular expression is error prone and emacs' regular expressions are limited
;; (for example, no lookaheads, named groups, etc.).
;; Using re-builder to interactively build regular expressions was a step into the right direction, but manually copying over the regexp
;; to the minibuffer is cumbersome.
;; Using the idea of interactive of of re-builder, this package makes it possible to use just the minibuffer to construct (with live visual feedback) the regexp and replacement,
;; using Emacs style regular expressions, or optionally, Python's regular expressions and Python expressions for the replacement.
;;
;; So a thanks to Detlev Zundel for his re-builder.

;;; Installation
;;
;; Put visual-regexp.el and regexp.py into the same directory.
;; Add the following code to your init file. Of course you can select
;; your own key bindings.
;; ----------------------------------------------------------
;; (add-to-list 'load-path "folder-in-which-visual-regexp-files-are-in/")
;; (require 'visual-regexp)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
;; ;; to use visual-regexp isearch instead of the built-in regexp isearch (invoked by `C-M-s`, `C-M-r`), also include the following lines:
;; (define-key esc-map (kbd "C-r") 'vr/isearch-backward)
;; (define-key esc-map (kbd "C-s") 'vr/isearch-forward)
;; ----------------------------------------------------------
;; To customize, use: M-x customize-group [RET] visual-regexp.
;; You can specify how to invoke the Python interpreter by modifying the vr/command-python variable. The default is "python /path/to/regexp.py".
;;
;; Execute C-h f "vr/replace" to read more and see examples.

;; Code goes here

(unless (fboundp 'make-overlay)
  (require 'overlay))

;; cl is used for (loop ...) macro
(require 'cl)

;;; faces

(defface vr/match-0
  '((((class color) (background light))
     :background "lightblue")
    (((class color) (background dark))
     :background "steelblue4")
    (t
     :inverse-video t))
  "First face for displaying a whole match."
  :group 'visual-regexp)

(defface vr/match-1
  '((((class color) (background light))
     :background "cadetblue")
    (((class color) (background dark))
     :background "dodgerblue4")
    (t
     :inverse-video t))
  "Second face for displaying a whole match."
  :group 'visual-regexp)

(defface vr/group-0
  '((((class color) (background light))
     :background "aquamarine")
    (((class color) (background dark))
     :background "blue3")
    (t
     :inverse-video t))
  "First face for displaying a matching group."
  :group 'visual-regexp)

(defface vr/group-1
  '((((class color) (background light))
     :background "springgreen")
    (((class color) (background dark))
     :background "chartreuse4")
    (t
     :inverse-video t))
  "Second face for displaying a matching group."
  :group 'visual-regexp)

(defface vr/group-2
  '((((min-colors 88) (class color) (background light))
     :background "yellow1")
    (((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "sienna4")
    (t
     :inverse-video t))
  "Third face for displaying a matching group."
  :group 'visual-regexp)

;;; variables

(defcustom vr/command-python (format "python %s" (expand-file-name "regexp.py" (file-name-directory load-file-name)))
  "External command used for the Python engine."
  :type 'string
  :group 'visual-regexp)

(defcustom vr/command-custom ""
  "Custom external command used when the engine is set to custom."
  :type 'string
  :group 'visual-regexp)

(defcustom vr/engine 'emacs
  "Which engine to use for searching/replacing. 
Use Emacs to use Emacs-style regular expressions.
Use Python to use Python's regular expressions (see vr/command-python).
Use Custom to use a custom external command (see vr/command-custom)."
  :type '(choice
	  (const :tag "Emacs" emacs) 
	  (const :tag "Python" python)
	  (const :tag "Custom" custom))
  :group 'visual-regexp)


;; (defcustom glasses-face nil
;;   "Face to be put on capitals of an identifier looked through glasses.
;; If it is nil, no face is placed at the capitalized letter.

;; For example, you can set `glasses-separator' to an empty string and
;; `glasses-face' to `bold'.  Then unreadable identifiers will have no separators,
;; but will have their capitals in bold."
;;   :group 'glasses
;;   :type '(choice (const :tag "None" nil) face)
;;   :set 'glasses-custom-set
;;   :initialize 'custom-initialize-default)

(defcustom vr/auto-show-help t
  "Show help message automatically when the minibuffer is entered."
  :type 'boolean
  :group 'visual-regexp)

(defcustom vr/default-feedback-limit 50
  "Limit number of matches shown in visual feedback. 
If nil, don't limit the number of matches shown in visual feedback."
  :type 'integer
  :group 'visual-regexp)

(defcustom vr/default-replace-preview nil
  "Preview of replacement activated by default? If activated, the original is not shown alongside the replacement."
  :type 'boolean
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

(defconst vr--match-faces '(vr/match-0 vr/match-1)
  "Faces in list for convenience")

(defconst vr--group-faces '(vr/group-0 vr/group-1 vr/group-2)
  "Faces in list for convenience")

(defconst vr--overlay-priority 1001
  "Starting priority of visual-regexp overlays.")

(defvar vr--in-minibuffer nil
  "Is visual-regexp currently being used?")

(defvar vr--last-minibuffer-contents nil
  "Keeping track of minibuffer changes")

(defvar vr--target-buffer-start nil
  "Starting position in target buffer.")

(defvar vr--target-buffer-end nil
  "Ending position in target buffer.")

(defvar vr--regexp-string nil
  "Entered regexp.")

(defvar vr--replace-string nil
  "Entered replacement.")

(defvar vr--use-expression nil
  "Use expression instead of string in replacement.")

(defvar vr--feedback-limit nil
  "Feedback limit currently in use.")

(defvar vr--replace-preview nil
  "Preview of replacement activated?")

(defvar vr--target-buffer nil
  "Buffer to which visual-regexp is applied to.")

(defvar vr--overlays (make-hash-table :test 'equal)
  "Overlays used in target buffer.")

(defvar vr--visible-overlays (list)
  "Overlays currently visible.")

;; modifiers IMSU (see http://docs.python.org/library/re.html#re.I)
(defvar vr--regexp-modifiers '()
  "Modifiers in use.")

;;; keymap

(defvar vr/minibuffer-regexp-keymap 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") (lambda () (interactive) (vr--minibuffer-help)))

    ;; C-i is also <tab>. http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
    (setq map (delq '(kp-tab . [9]) map))
    ;;(keyboard-translate ?\C-i ?\H-i)
    (define-key map (kbd "C-c i") (lambda () (interactive) (vr--toggle-regexp-modifier :I)))
    (define-key map (kbd "C-c m") (lambda () (interactive) (vr--toggle-regexp-modifier :M)))
    (define-key map (kbd "C-c s") (lambda () (interactive) (vr--toggle-regexp-modifier :S)))
    (define-key map (kbd "C-c u") (lambda () (interactive) (vr--toggle-regexp-modifier :U)))

    (define-key map (kbd "C-c a") 'vr--toggle-limit)
    map)
  "Keymap used while using visual-regexp,")

(defvar vr/minibuffer-replace-keymap 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") (lambda () (interactive) (vr--minibuffer-help)))
    (define-key map (kbd "C-c C-c") (lambda () (interactive)
				      (when (not (eq vr/engine 'emacs))
					(when (equal vr--in-minibuffer 'vr--minibuffer-replace)
					  (setq vr--use-expression (not vr--use-expression))
					  (vr--update-minibuffer-prompt)
					  (vr--do-replace-feedback)))))
    (define-key map (kbd "C-c m") (lambda ()
				    (interactive)
				    (when (equal vr--in-minibuffer 'vr--minibuffer-replace)
				      (vr--delete-overlay-displays)
				      ;; wait for any input to redisplay replacements
				      (sit-for 100000000 t)
				      (vr--do-replace-feedback))))
    (define-key map (kbd "C-c p") (lambda ()
				    (interactive)
				    (when (equal vr--in-minibuffer 'vr--minibuffer-replace)
				      (setq vr--replace-preview (not vr--replace-preview))
				      (vr--update-minibuffer-prompt)
				      (vr--do-replace-feedback))))
    
    (define-key map (kbd "C-c a") 'vr--toggle-limit)
    map)
  "Keymap used while using visual-regexp,")


;;; helper functions

(defun vr--get-command ()
  (cond
   ((eq vr/engine 'python) vr/command-python)
   ((eq vr/engine 'custom) vr/command-custom)))

(defun vr--toggle-limit ()
  "Toggle the limit of overlays shown (default limit / no limit)"
  (interactive)
  (if vr--feedback-limit
      (setq vr--feedback-limit nil)
    (setq vr--feedback-limit vr/default-feedback-limit))
  (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
	 (vr--regexp-feedback))
	((equal vr--in-minibuffer 'vr--minibuffer-replace)
	 (vr--regexp-feedback t) ;; update overlays
	 (vr--do-replace-feedback))))

(defun vr--regexp-modifiers-enabled ()
  (eq vr/engine 'python))

(defun vr--toggle-regexp-modifier (modifier)
  "modifier should be one of :I, :M, :S, :U."
  (plist-put vr--regexp-modifiers modifier 
	     (not (plist-get vr--regexp-modifiers modifier)))
  (vr--update-minibuffer-prompt)
  (vr--regexp-feedback))

(defun vr--get-regexp-string ()
  (concat (vr--get-regexp-modifiers-prefix) 
	  (if (equal vr--in-minibuffer 'vr--minibuffer-regexp) 
	      (minibuffer-contents-no-properties) 
	    vr--regexp-string)))

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
    "")
  )

;;; minibuffer functions

(defun vr--minibuffer-set-prompt (prompt)
  "Updates minibuffer prompt. Call when minibuffer is active."
  (let ((inhibit-read-only t)) 
    (put-text-property (point-min) (minibuffer-prompt-end) 'display prompt)))

(defun vr--update-minibuffer-prompt ()
  (when (and vr--in-minibuffer (minibufferp))
    (vr--minibuffer-set-prompt
     (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
	    (format "Regexp: %s" (vr--get-regexp-modifiers-prefix)))
	   ((equal vr--in-minibuffer 'vr--minibuffer-replace)
	    (concat
	     "Replace"
	     (let ((flag-infos (mapconcat 'identity 
					  (delq nil (list (when vr--use-expression "using expression")
							  (when vr--replace-preview "preview"))) 
					  ", ")))
	       (when (not (string= "" flag-infos ))
		 (format " (%s)" flag-infos)))
	     (format " (%s)" (vr--get-regexp-string))
	     ": "))))))

(defun vr--minibuffer-message (message)
  "Minibuffer message without timeout"
  (let ((minibuffer-message-timeout nil))
    (minibuffer-message message)))

(defun vr--minibuffer-help ()
  (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
	 (vr--minibuffer-message (format "C-c ?: help,%s C-c a: toggle show all" (if (vr--regexp-modifiers-enabled) " C-c i: toggle case, C-c m: toggle multiline match of ^ and $, C-c s: toggle dot matches newline," ""))))
	((equal vr--in-minibuffer 'vr--minibuffer-replace)
	 (vr--minibuffer-message (format "C-c ?: help,%s C-c m: show matches/groups, C-c p: toggle preview, C-c a: toggle show all" (if (not (eq vr/engine 'emacs)) " C-c C-c: toggle expression," ""))))))

;;; overlay functions

(defun vr--get-overlay (i j)
  "i: match index, j: submatch index"
  (let (overlay)
    (setq overlay (gethash (list i j) vr--overlays))
    (unless overlay ;; create new one if overlay does not exist yet
      (progn 
	(setq overlay (make-overlay 0 0))
	(if (= 0 j)
	    (overlay-put overlay 'face (nth (mod i (length vr--match-faces)) vr--match-faces))
	  (overlay-put overlay 'face (nth (mod j (length vr--group-faces)) vr--group-faces)))
	(overlay-put overlay 'priority (+ vr--overlay-priority (if (= j 0) 0 1)))
	(overlay-put overlay 'vr-ij (list i j))
	(when (= 0 j)
	  (overlay-put overlay 'intangible t))
	(puthash (list i j) overlay vr--overlays)
	))
    overlay))

(defun vr--delete-overlays ()
  "Delete all visible overlays."
  (mapc (lambda (overlay)
	     (delete-overlay overlay)) 
	   vr--visible-overlays)
  (setq vr--visible-overlays (list)))

(defun vr--delete-overlay-displays ()
  "Delete the display of all visible overlays. Call before vr--delete-overlays."
  (mapc (lambda (overlay)
	  (multiple-value-bind (i j) (overlay-get overlay 'vr-ij)
	    (when (= 0 j)
	      (overlay-put overlay 'display nil)
	      (overlay-put overlay 'after-string nil)
	      (overlay-put overlay 'priority vr--overlay-priority))))
	vr--visible-overlays))

;;; hooks

(defun vr--update (beg end len)
  (when (and vr--in-minibuffer (minibufferp))
    ;; minibuffer-up temporarily deletes minibuffer contents before inserting new one.
    ;; don't do anything then as the messages shown by visual-regexp are irritating while browsing the history.
    (unless (and (string= "" (minibuffer-contents-no-properties))
		 (equal last-command 'previous-history-element))
      ;; do something when minibuffer contents changes
      (unless (string= vr--last-minibuffer-contents (minibuffer-contents-no-properties))
	(setq vr--last-minibuffer-contents (minibuffer-contents-no-properties))
	;; minibuffer contents has changed, update visual feedback.
	;; not using after-change-hook because this hook applies to the whole minibuffer, including minibuffer-messages
	;; that disappear after a while.
	(cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
	       (vr--regexp-feedback))
	      ((equal vr--in-minibuffer 'vr--minibuffer-replace)
	       (vr--do-replace-feedback)))))))
(add-hook 'after-change-functions 'vr--update)

(defun vr--minibuffer-setup ()
  "Setup prompt and help when entering minibuffer."
  (when vr--in-minibuffer
    (progn
      (vr--update-minibuffer-prompt)
      (when vr/auto-show-help (vr--minibuffer-help)))))
(add-hook 'minibuffer-setup-hook 'vr--minibuffer-setup)

;;; shell command / parsing functions
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
	   (message "script failed:%s\n" output)))))

(defun vr--not-last-line () 
  "Output of external script ends in one line of message and one empty line.
Return t if current line is not the line with the message."
  (save-excursion (= 0 (forward-line 2))))

(defun vr--current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun vr--unescape (s)
  "Replacement strings returned by external script have escaped newlines and backslashes (so that there can be one replacement per line). Unescape to get back original.
Escaped newlines are only unescaped if newline is not nil."
  (setq s (replace-regexp-in-string (regexp-quote "\\n") (regexp-quote "\n") s))
  (replace-regexp-in-string (regexp-quote "\\\\") (regexp-quote "\\") s))

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
	      (setq replacements (cons (list replacement begin end i) replacements)))
	    (forward-line 1))
      (setq message-line (vr--unescape (vr--current-line))))
    (list replacements message-line)))

;;; helper functions

(defun vr--target-window ()
  (if vr--target-buffer
      (get-buffer-window vr--target-buffer)
    nil))

(defun vr--compose-messages (&rest msgs)
  (mapconcat 'identity (delq nil (mapcar (lambda (msg) (if (or (not msg) (string= "" msg)) nil msg)) msgs)) " - "))

;;; show feedback functions
(defun vr--regexp-feedback-function ()
  (cond
   ((eq vr/engine 'emacs) (vr--regexp-feedback-function-emacs))
   (t (vr--regexp-feedback-function-external))))

(defun vr--regexp-feedback-function-external ()
  "Feedback function for search using an external command."
  (vr--run-command 
   (format "%s matches --regexp %s %s" (vr--get-command) (shell-quote-argument (vr--get-regexp-string)) (when vr--feedback-limit (format "--feedback-limit %s" vr--feedback-limit)))
   (lambda (output)
     (vr--parse-matches
      output 
      'vr--regexp-feedback-match-callback))))

(defun vr--regexp-feedback-function-emacs ()
  "Feedback function for emacs-style regexp search"
  (let ((regexp-string (vr--get-regexp-string))
	(message-line "")
	(err))
    (with-current-buffer vr--target-buffer
      (save-excursion
	(goto-char vr--target-buffer-start)
	(let ((i 0)
	      (looping t))
	  (while (and looping
		      (not limit-reached)
		      (condition-case err
			  (re-search-forward regexp-string vr--target-buffer-end t)
			('invalid-regexp (progn (setq message-line (car (cdr err))) nil))))
	    (loop for (start end) on (match-data) by 'cddr
		  for j from 0 do
		  (vr--regexp-feedback-match-callback i j start end))
	    (when (= (match-beginning 0) (match-end 0))
	      (if (> vr--target-buffer-end (point))
		  (forward-char) ;; don't get stuck on zero-width matches
		(setq looping nil)))
	    (setq i (1+ i))))))
    message-line))

(defun vr--regexp-feedback-match-callback (i j begin end)
  (with-current-buffer vr--target-buffer
  (save-excursion
    (when (= 0 i) ;; first match: if invisible, make it visible.
      (with-selected-window (vr--target-window)
	(if (>= begin (window-end nil t))
	    (goto-char begin))))
    (let ((overlay (vr--get-overlay i j)))
      (move-overlay overlay begin end vr--target-buffer)
      (if (and (= 0 j) (= begin end)) ;; empty match; indicate by a pipe
	  (overlay-put overlay 'after-string (propertize "|" 'face (nth (mod i (length vr--match-faces)) vr--match-faces) 'help-echo "empty match"))
	(overlay-put overlay 'after-string nil))
      (setq vr--visible-overlays (cons overlay vr--visible-overlays)))
    ;; mark if we have reached the specified feedback limit	  
    (when (and vr--feedback-limit (= vr--feedback-limit (1+ i)) )
      (setq limit-reached t)))))

(defun vr--regexp-feedback (&optional inhibit-message)
  "Show visual feedback for matches."
  (vr--delete-overlays)
  (let ((limit-reached nil) 
	message-line)
    (setq message-line 
	  (vr--regexp-feedback-function))
    (unless inhibit-message
      (let ((msg (vr--compose-messages message-line (when limit-reached (format "%s matches shown, hit C-c a to show all" vr/default-feedback-limit)))))
	(unless (string= "" msg)
	  (vr--minibuffer-message msg))))))

(defun vr--do-replace-feedback-match-callback (replacement begin end i)
  (let* ((overlay (vr--get-overlay i 0))
	 (empty-match (equal (overlay-start overlay) (overlay-end overlay))))
    (let ((original (with-current-buffer vr--target-buffer (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))))
      (if (or empty-match vr--replace-preview)
	  (progn 
	    (overlay-put overlay (if empty-match 'after-string 'display) (propertize (vr--unescape replacement) 'face (nth (mod i (length vr--match-faces)) vr--match-faces)))
	    (overlay-put overlay 'priority (+ vr--overlay-priority 2)))
	(progn
	  ;;(overlay-put overlay 'display nil)
	  (overlay-put overlay 'after-string 
		       (propertize (format " => %s" (vr--unescape replacement)) 'face (nth (mod i (length vr--match-faces)) vr--match-faces)))
	  (overlay-put overlay 'priority (+ vr--overlay-priority 0)))))))

(defun vr--get-replacements (feedback feedback-limit)
  (cond
   ((eq vr/engine 'emacs) (vr--get-replacements-emacs feedback feedback-limit))
   (t (vr--get-replacements-external feedback feedback-limit))))

(defun vr--get-replacements-external (feedback feedback-limit)
  "Get replacements using an external command."
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
   'vr--parse-replace))

(defun vr--get-replacements-emacs (feedback feedback-limit)
  "Get replacements using emacs-style regexp."
  (let ((regexp-string (vr--get-regexp-string))
	(message-line "")
	(replacements (list))
	(err)
	(buffer-contents (with-current-buffer vr--target-buffer
			   (buffer-substring-no-properties (point-min) (point-max)))))
        
    (with-temp-buffer ;; todo: user vr--target-buffer, but goto-char and re-search-forward go crazy when overlays are visible...
      (insert buffer-contents)
      (goto-char vr--target-buffer-start)
      	(let ((i 0)
	      (looping t))
	  (while (and
		  looping
		  (or (not feedback) (not feedback-limit) (< i feedback-limit))
		  (condition-case err
		      (re-search-forward regexp-string vr--target-buffer-end t)
		    ('invalid-regexp (progn (setq message-line (car (cdr err))) nil))))
	    (condition-case err
		(progn
		  (setq replacements (cons
				      (list (match-substitute-replacement replace-string) (match-beginning 0) (match-end 0) i)
				      replacements))
		  (when (= (match-beginning 0) (match-end 0))
		    (if (> vr--target-buffer-end (point))
			(forward-char) ;; don't get stuck on zero-width matches
		      (setq looping nil)))
		  (setq i (1+ i)))
	      ('error (progn
			(setq message-line (car (cdr err)))
			(setq replacements (list))
			(setq looping nil)))))))
    (list replacements message-line)))

(defun vr--do-replace-feedback ()
  "Show visual feedback for replacements."
  (vr--regexp-feedback t) ;; only really needed when regexp has not been changed from default (=> no overlays have been created)
  (vr--delete-overlay-displays)
  (let ((replace-string (minibuffer-contents-no-properties)))
    (multiple-value-bind (replacements message-line) (vr--get-replacements t vr--feedback-limit)
      ;; visual feedback for matches
      (mapc (lambda (replacement-info) (apply 'vr--do-replace-feedback-match-callback replacement-info)) replacements)
      (unless (string= "" message-line)
	(vr--minibuffer-message message-line)))))

;;; replace / vr/replace

(defun vr--do-replace (&optional silent)
  "Replace matches."
  (vr--delete-overlay-displays)
  (vr--delete-overlays)
  (let ((replace-string vr--replace-string))
    (multiple-value-bind (replacements message-line) (vr--get-replacements nil nil)
      (let ((replace-count 0)
	    (cumulative-offset 0)
	    match-data)
	(loop for replacement-info in replacements 
	      for counter from 0 do 
	      (setq replace-count (1+ replace-count))
	      (multiple-value-bind (replacement begin end i) replacement-info
		;; replace match
		(let ((replacement (vr--unescape replacement)))
		  (with-current-buffer vr--target-buffer
		    (save-excursion
		      ;; first insert, then delete
		      ;; this ensures that if we had an active region before, the replaced match is still part of the region
		      (goto-char begin)
		      (insert replacement)
		      (setq cumulative-offset (+ cumulative-offset (- (point) end)))
		      (delete-char (- end begin)))))
		(when (= 0 counter)
		  (setq match-data (list begin end)))))
	(unless (or silent (string= "" message-line))
	  (vr--minibuffer-message message-line))
	(set-match-data (list (+ cumulative-offset (nth 0 match-data)) (+ cumulative-offset (nth 1 match-data))))
	replace-count))))

(defun vr--interactive-get-args ()
  "Get interactive args for the vr/replace function."
  (unwind-protect 
      (progn
	(let ((buffer-read-only t)) ;; make target buffer
	  (when vr--in-minibuffer (error "visual-regexp already in use."))
	  (setq vr--target-buffer (current-buffer))
	  (setq vr--use-expression current-prefix-arg)
	  (setq vr--target-buffer-start (if (and transient-mark-mode mark-active) 
						 (region-beginning)
					       (point)))
	  (setq vr--target-buffer-end (if (and transient-mark-mode mark-active) 
					       (region-end)
					     (point-max)))

	  (setq vr--feedback-limit vr/default-feedback-limit)
	  (setq vr--regexp-modifiers (copy-sequence vr/default-regexp-modifiers))
	  (setq vr--replace-preview vr/default-replace-preview)

	  (save-excursion
	    ;; deactivate mark so that we can see our faces instead of region-face.
	    (deactivate-mark)
	    (progn 
	      (setq vr--in-minibuffer 'vr--minibuffer-regexp)
	      (setq vr--last-minibuffer-contents "")
	      (setq vr--regexp-string 
		    (read-from-minibuffer 
		     " " ;; prompt will be  set in vr--minibuffer-setup
		     nil vr/minibuffer-regexp-keymap))
	      ;;(setq vr--regexp-string (format "%s%s" (vr--get-regexp-modifiers-prefix) vr--regexp-string))
	      
	      (setq vr--in-minibuffer 'vr--minibuffer-replace)
	      (setq vr--last-minibuffer-contents "")
	      (setq vr--replace-string
		    (read-from-minibuffer 
		     " " ;; prompt will be  set in vr--minibuffer-setup 
		     nil vr/minibuffer-replace-keymap))))
	  ;; Successfully got the args, deactivate mark now. If the command was aborted (C-g), the mark (region) would remain active.
	  (deactivate-mark)
	  (list vr--regexp-string 
		vr--replace-string
		vr--target-buffer-start
		vr--target-buffer-end
		vr--regexp-modifiers
		vr--use-expression)))
    (progn ;; execute on finish
      (setq vr--in-minibuffer nil)
      (vr--delete-overlay-displays)
      (vr--delete-overlays))))

(defun vr/replace (regexp replace start end &optional modifiers use-expression)
  "Regexp-replace with interactive feedback, using Python regular expressions. 
When used interactively with prefix arg, the replacement string is a Python expression. The Python expression has access to the following variables:
- i: the index of the match
- m: the match object
- \\0, \\1, ...: captured groups (those are aliases for m.group(0), m.group(1), ...).

Example 1: 
regexp: abcd(.)(.)
replace: abc\\2\\1

Example 2: capitalize every word (use prefix arg to use a Python expression)
regexp: \\b\\w
replace: \\0.upper()

Example 3: enumerate all words and put them on new lines (use prefix arg to use a Python expression)
regexp: \\w+
replace: \"\\n{}. {}\".format(i+1, \\0)
"
  (interactive 
   (vr--interactive-get-args))
  (unwind-protect 
      (progn 
	(when vr--in-minibuffer (error "visual-regexp already in use."))
	(setq vr--target-buffer (current-buffer)
	      vr--target-buffer-start start
	      vr--target-buffer-end end
	      vr--regexp-modifiers (if modifiers modifiers (copy-sequence vr/default-regexp-modifiers))
	      vr--use-expression use-expression
	      vr--regexp-string regexp
	      vr--replace-string replace)
	;; do replacement
	(vr--do-replace))
    ;; execute on finish
    (setq vr--in-minibuffer nil)))


;; isearch starts here

(defun vr/isearch-forward()
  "Like isearch-forward, but using Python regular expressions."
  (interactive)
  (let ((isearch-search-fun-function 'vr/isearch-search-fun-function))
    (isearch-forward-regexp)))

(defun vr/isearch-backward()
  "Like isearch-backward, but using Python regular expressions."
  (interactive)
  (let ((isearch-search-fun-function 'vr/isearch-search-fun-function))
    (isearch-backward-regexp)))

(defvar vr--isearch-cache-key nil)
(defvar vr--isearch-cache-val nil)

(defun vr--isearch-forward (string &optional bound noerror count)
  (vr--isearch t string bound noerror count))

(defun vr--isearch-backward (string &optional bound noerror count)
  (vr--isearch nil string bound noerror count))

(defun vr--isearch-find-match (matches start)
  (let ((i (vr--isearch-find-match-bsearch matches start 0 (- (length matches) 1))))
    (unless (eq i -1)
      (aref matches i))))

(defun vr--isearch-find-match-bsearch (matches start left right)
  (if (= 0 (length matches))
      -1
    (let ((mid (/ (+ left right) 2))
	  (el (if forward 0 1)) ;; 0 => beginning of match; 1 => end of match
	  (cmp (if forward '<= '>=)))
      (cond ((eq left right)
	     (if (funcall cmp start (nth el (aref matches mid)))
		 left
	       -1)
	     )
	    ((funcall cmp start (nth el (aref matches mid)))
	     (vr--isearch-find-match-bsearch matches start left mid))
	    (t
	     (vr--isearch-find-match-bsearch matches start (1+ mid) right))))))

(defun vr--isearch (forward string &optional bound noerror count)
  ;; This is be called from isearch. In the first call, bound will be nil to find the next match.
  ;; Afterwards, lazy highlighting kicks in, which calls this function many times, for different values of (point), always with the same bound (window-end (selected-window)).
  ;; Calling a process repeatedly is noticeably  slow. To speed the lazy highlighting up, we fetch all matches in the visible window at once and cache them for subsequent calls.
  (let* ((is-called-from-lazy-highlighting bound) ;; we assume only lazy highlighting sets a bound. isearch does not, and neither does our own vr/query-replace.
	 matches-vec ;; stores matches from regexp.py
	 message-line ;; message from regexp.py
	 (regexp (if case-fold-search (concat "(?i)" string) string))
	 (start
	  (if forward 
	      (if is-called-from-lazy-highlighting (window-start (selected-window)) (point))
	    (if is-called-from-lazy-highlighting bound (point-min))))
	 (end
	  (if forward
	      (if is-called-from-lazy-highlighting bound (point-max))
	    (if is-called-from-lazy-highlighting (window-end (selected-window)) (point))))
	 (cache-key (list regexp start end)))
    (if (and is-called-from-lazy-highlighting (equal vr--isearch-cache-key cache-key))
	(setq matches-vec vr--isearch-cache-val) ;; cache hit
      (progn ;; no cache hit, populate matches-vec
	(setq vr--target-buffer-start start
	      vr--target-buffer-end end
	      vr--target-buffer (current-buffer))
	(vr--run-command 
	 (format "%s matches --regexp %s %s %s" 
		 (vr--get-command) (shell-quote-argument regexp) 
		 (if count
		     (format "--feedback-limit %s" count)
		   ;; if no bound, the rest of the buffer is searched for the first match -> need only one match
		   (if bound "" "--feedback-limit 1")) 
		 (if forward "" "--backwards"))
	 (lambda (output)
	   ;; initialize matches-vec
	   (setq matches-vec (make-vector 
			      (with-temp-buffer (insert output) (- (line-number-at-pos (point-max)) 2)) ;; number of matches
			      nil))
	   (let ((cur-match (list)))
	     (setq message-line
		   ;; populate matches-vec 
		   (vr--parse-matches
		    output 
		    (lambda (i j begin end) 
		      (progn
			(when (and (= j 0) (> i 0))
			  (aset matches-vec (- i 1) (nreverse cur-match))
			  (setq cur-match (list)))
			(setq cur-match (cons end (cons begin cur-match)))))))
	     (when cur-match
	       (aset matches-vec (- (length matches-vec) 1) (nreverse cur-match))))
	   (when is-called-from-lazy-highlighting ;; store in cache
	     (setq vr--isearch-cache-key cache-key
		   vr--isearch-cache-val matches-vec))))))
    (let ((match (vr--isearch-find-match matches-vec (point))))
      (if match
	  (progn
	    (set-match-data (mapcar 'copy-marker match)) ;; needed for isearch 
	    (if forward
		(goto-char (nth 1 match)) ;; move to end of match
	      (goto-char (nth 0 match)) ;; move to beginning of match
	      ))
	(progn 
	  (set-match-data (list 0 0))
	  (when (string= "Invalid:" (substring message-line 0 8))
	    (signal 'invalid-regexp (list message-line))))))))

(defun vr/isearch-search-fun-function ()
  "To enable vr/isearch, set isearch-search-fun-function to vr/isearch-search-fun-function, i.e. `(setq isearch-search-fun-function 'vr/isearch-search-fun-function)`."
  ;; isearch-search-fun is a function that returns the function that does the search. it calls isearch-search-fun-function (if it exists) to do its job.
  (if isearch-regexp ;; let us handle regexp search
      (if isearch-forward 'vr--isearch-forward 'vr--isearch-backward)
    (let ((isearch-search-fun-function nil)) ;; fall back to the default implementation of isearch, which will handle regular search and word search.
      (isearch-search-fun))))

(add-hook 'isearch-mode-end-hook (lambda ()
				   (setq vr--isearch-cache-key nil
					 vr--isearch-cache-val nil)))


;; query-replace-regexp starts here

(defvar vr--query-replacements nil)
;; we redefine the help text from replace.el to remove the commands we don't support.

(defconst vr--query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r [not supported in visual-regexp],
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ [not supported in visual-regexp],
E [not supported in visual-regexp]"
  "Help message while in `vr/query-replace'.")

(defvar vr--query-replace-map 
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map query-replace-map)
    ;; the following replace.el commands are not supported by visual-regexp.
    (define-key map "e" nil)
    (define-key map "E" nil)
    (define-key map "\C-r" nil)
    (define-key map "\C-w" nil)
    (define-key map "^" nil)
    map
    ))

(defun vr/query-replace (regexp replace start end &optional modifiers use-expression)
  "Use vr/query-replace like you would use query-replace-regexp."
  (interactive 
   (vr--interactive-get-args))

  (unwind-protect 
      (progn 
	(when vr--in-minibuffer (error "visual-regexp already in use."))
	(setq vr--target-buffer (current-buffer)
	      vr--target-buffer-start start
	      vr--target-buffer-end end
	      vr--regexp-modifiers (if modifiers modifiers (copy-sequence vr/default-regexp-modifiers))
	      vr--use-expression use-expression
	      vr--regexp-string regexp
	      vr--replace-string replace)
	(let ((replace-string vr--replace-string))
	  (setq vr--query-replacements (nreverse (car (vr--get-replacements nil nil)))))
	(vr--perform-replace regexp nil))
    ;; execute on finish
    (setq vr--in-minibuffer nil)))

(defun vr--perform-replace (from-string &optional map)
  ;; This function is a heavily modified version of (perform-replace) from replace.el.
  ;; The original plan was to use the original perform-replace, but various issues stood in the way.
  (or map (setq map vr--query-replace-map))
  (and minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let* (
	 ;; isearch is used for highlighting. plug in our search function such that highlighting works correctly.
	 (isearch-search-fun-function 'vr/isearch-search-fun-function)
	 (search-function 'vr--isearch-forward)
	 (query-replace-help vr--query-replace-help)
	 (search-string from-string)
	 (real-match-data nil) ; The match data for the current match.
	 (next-replacement nil) ;; replacement string for current match
	 (keep-going t)
	 (replace-count 0)
	 (automatic nil)
	 ;; a match can be replaced by a longer/shorter replacement. cumulate the difference
	 (cumulative-offset 0)
	 (end vr--target-buffer-end)
	 (recenter-last-op nil)	; Start cycling order with initial position.
	 (message
	  (apply 'propertize
		 (substitute-command-keys
		  "Query replacing %s with %s: (\\<vr--query-replace-map>\\[help] for help) ")
		 minibuffer-prompt-properties)))

    (when query-replace-lazy-highlight
      (setq isearch-lazy-highlight-last-string nil))

    (goto-char vr--target-buffer-start)
    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going vr--query-replacements)
	  ;; Advance replacement list
	  (multiple-value-bind (replacement begin end) (car vr--query-replacements)
	    (set-match-data (mapcar 'copy-marker (list (+ cumulative-offset begin) (+ cumulative-offset end))))
	    (setq next-replacement replacement))
	  (goto-char (match-end 0))
	  (setq real-match-data (replace-match-data t real-match-data))
	  (setq vr--query-replacements (cdr vr--query-replacements))

	  (undo-boundary)
	  (let (done replaced key def)
	    ;; Loop reading commands until one of them sets done,
	    ;; which means it has finished handling this
	    ;; occurrence. 
	    ;; Commands not setting `done' need to adjust
	    ;; `real-match-data'.
	    (while (not done)
	      (condition-case nil
		  ;; number of arguments to replace-highlight changed
		  ;; http://bzr.savannah.gnu.org/lh/emacs/emacs-24/revision/109900
		  ;; try both versions
		  (replace-highlight (match-beginning 0) (match-end 0) start end search-string t nil nil)
		('error (replace-highlight (match-beginning 0) (match-end 0) start end search-string t nil)))
	      ;; Bind message-log-max so we don't fill up the message log
	      ;; with a bunch of identical messages.
	      (let ((message-log-max nil))
		(message message
			 (query-replace-descr from-string)
			 (query-replace-descr next-replacement)))
	      (setq key (read-event))
	      ;; Necessary in case something happens during read-event
	      ;; that clobbers the match data.
	      (set-match-data real-match-data)
	      (setq key (vector key))
	      (setq def (lookup-key map key))
	      ;; Restore the match data while we process the command.
	      (set-match-data real-match-data)
	      (cond ((eq def 'help)
		     (with-output-to-temp-buffer "*Help*"
		       (princ
			(concat "Query replacing visual-regexp "
				from-string " with "
				next-replacement ".\n\n"
				(substitute-command-keys
				 query-replace-help)))
		       (with-current-buffer standard-output
			 (help-mode))))
		    ((eq def 'exit)
		     (setq keep-going nil
			   done t))
		    ((eq def 'act)
		     (unless replaced
		       (replace-match next-replacement t t)
		       (setq replace-count (1+ replace-count)))
		     (setq done t 
			   replaced t))
		    ((eq def 'act-and-exit)
		     (unless replaced
		       (replace-match next-replacement t t)
		       (setq replace-count (1+ replace-count)))
		     (setq keep-going nil
			   done t r
			   eplaced t))
		    ((eq def 'act-and-show)
		     (unless replaced
		       (replace-match next-replacement t t)
		       (setq replace-count (1+ replace-count)
			     real-match-data (replace-match-data t real-match-data))
		       (setq replaced t)))
		    ((eq def 'automatic)
		     (setq vr--target-buffer-start (match-beginning 0)
			   vr--target-buffer-end (+ cumulative-offset end))
		     (setq replace-count (+ replace-count (vr--do-replace t)))
		     (setq done t 
			   replaced t 
			   keep-going nil))
		    ((eq def 'skip)
		     (setq done t))
		    ((eq def 'recenter)
		     ;; `this-command' has the value `query-replace',
		     ;; so we need to bind it to `recenter-top-bottom'
		     ;; to allow it to detect a sequence of `C-l'.
		     (let ((this-command 'recenter-top-bottom)
			   (last-command 'recenter-top-bottom))
		       (recenter-top-bottom)))
		    (t
		     (setq this-command 'mode-exited)
		     (setq keep-going nil)
		     (setq unread-command-events
			   (append (listify-key-sequence key)
				   unread-command-events))
		     (setq done t)))
	      (when replaced
		(setq cumulative-offset (+ cumulative-offset (- (length next-replacement) (- (nth 1 real-match-data) (nth 0 real-match-data))))))
	      (when query-replace-lazy-highlight
		;; Force lazy rehighlighting only after replacements.
		(if (not (memq def '(skip backup)))
		    (setq isearch-lazy-highlight-last-string nil)))
	      (unless (eq def 'recenter)
		;; Reset recenter cycling order to initial position.
		(setq recenter-last-op nil)))))
            
      (replace-dehighlight))
    (unless unread-command-events
      ;; point is set to the end of the last occurrence.
      (goto-char (match-end 0))
      (message "Replaced %d occurrence%s"
	       replace-count
	       (if (= replace-count 1) "" "s")))))


(provide 'visual-regexp)

;;; visual-regexp.el ends here

