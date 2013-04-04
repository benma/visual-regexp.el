;;; visual-regexp.el --- A regexp/replace command for Emacs with interactive visual feedback

;; Copyright (C) 2012-2013 Marko Bencun

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
;;(require 'visual-ext-regexp)

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

(defvar vr/initialize-hook nil
  "Hook called before vr/replace and vr/query-replace")

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

;;; keymap

(defvar vr/minibuffer-regexp-keymap 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") (lambda () (interactive) (vr--minibuffer-help)))

    (define-key map (kbd "C-c a") 'vr--toggle-limit)
    map)
  "Keymap used while using visual-regexp,")

(defvar vr/minibuffer-replace-keymap 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") (lambda () (interactive) (vr--minibuffer-help)))
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

(defun vr--toggle-limit ()
  "Toggle the limit of overlays shown (default limit / no limit)"
  (interactive)
  (if vr--feedback-limit
      (setq vr--feedback-limit nil)
    (setq vr--feedback-limit vr/default-feedback-limit))
  (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
	 (vr--feedback))
	((equal vr--in-minibuffer 'vr--minibuffer-replace)
	 (vr--feedback t) ;; update overlays
	 (vr--do-replace-feedback))))


(defun vr--get-regexp-string ()
  (concat (if (equal vr--in-minibuffer 'vr--minibuffer-regexp) 
	      (minibuffer-contents-no-properties) 
	    vr--regexp-string)))


;;; minibuffer functions

(defun vr--minibuffer-set-prompt (prompt)
  "Updates minibuffer prompt. Call when minibuffer is active."
  (let ((inhibit-read-only t)) 
    (put-text-property (point-min) (minibuffer-prompt-end) 'display prompt)))

(defun vr--set-minibuffer-prompt-regexp ()
  ;; (format "Regexp: %s" (vr--get-regexp-modifiers-prefix))
  "Regexp: "
  )

(defun vr--set-minibuffer-prompt-replace ()
  (concat "Replace"
	  (let ((flag-infos (mapconcat 'identity 
				       (delq nil (list (when vr--replace-preview "preview"))) 
				       ", ")))
	    (when (not (string= "" flag-infos ))
	      (format " (%s)" flag-infos)))
	  (format " (%s)" (vr--get-regexp-string))
	  ": "))

(defun vr--update-minibuffer-prompt ()
  (when (and vr--in-minibuffer (minibufferp))
    (vr--minibuffer-set-prompt
     (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
	    (vr--set-minibuffer-prompt-regexp))
	   ((equal vr--in-minibuffer 'vr--minibuffer-replace)
	    (vr--set-minibuffer-prompt-replace))))))

(defun vr--minibuffer-message (message)
  "Minibuffer message without timeout"
  (let ((minibuffer-message-timeout nil))
    (minibuffer-message message)))

(defun vr/minibuffer-help-regexp ()
  (vr--minibuffer-message (format "C-c ?: help,%s C-c a: toggle show all" (if nil " C-c i: toggle case, C-c m: toggle multiline match of ^ and $, C-c s: toggle dot matches newline," "")))
  )

(defun vr/minibuffer-help-replace ()
  (vr--minibuffer-message (format "C-c ?: help,%s C-c m: show matches/groups, C-c p: toggle preview, C-c a: toggle show all" (if nil " C-c C-c: toggle expression," "")))
  )

(defun vr--minibuffer-help ()
  (cond ((equal vr--in-minibuffer 'vr--minibuffer-regexp)
	 (vr/minibuffer-help-regexp))
	((equal vr--in-minibuffer 'vr--minibuffer-replace)
	 (vr/minibuffer-help-replace))))

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
	       (vr--feedback))
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



;;; helper functions

(defun vr--target-window ()
  (if vr--target-buffer
      (get-buffer-window vr--target-buffer)
    nil))

(defun vr--compose-messages (&rest msgs)
  (mapconcat 'identity (delq nil (mapcar (lambda (msg) (if (or (not msg) (string= "" msg)) nil msg)) msgs)) " - "))

;;; show feedback functions
(defun vr--feedback-function (forward feedback-limit callback)
  "Feedback function for emacs-style regexp search"
  (let ((message-line "")
	(err))
    (with-current-buffer vr--target-buffer
      (save-excursion
	(goto-char (if forward vr--target-buffer-start vr--target-buffer-end))
	(let ((i 0)
	      (looping t))
	  (while (and looping
		      (or (not feedback-limit) (< i feedback-limit))
		      (condition-case err
			  (if forward
			      (re-search-forward regexp-string vr--target-buffer-end t)
			    (re-search-backward regexp-string vr--target-buffer-start t))
			('invalid-regexp (progn (setq message-line (car (cdr err))) nil))))
	    (loop for (start end) on (match-data) by 'cddr
		  for j from 0 do
		  (funcall callback i j start end))
	    (when (= (match-beginning 0) (match-end 0))
	      (cond ;; don't get stuck on zero-width matches
	       ((and forward (> vr--target-buffer-end (point))) (forward-char))
	       ((and (not forward) (< vr--target-buffer-start (point))) (backward-char))
	       (t (setq looping nil))))
	    (setq i (1+ i))))))
    message-line))

(defun vr--feedback-match-callback (i j begin end)
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

(defun vr--feedback (&optional inhibit-message)
  "Show visual feedback for matches."
  (vr--delete-overlays)
  (let ((limit-reached nil) 
	message-line)
    (setq message-line
	  (let ((regexp-string (vr--get-regexp-string)))
	    (vr--feedback-function t vr--feedback-limit 'vr--feedback-match-callback)))
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
	    (overlay-put overlay (if empty-match 'after-string 'display) (propertize replacement 'face (nth (mod i (length vr--match-faces)) vr--match-faces)))
	    (overlay-put overlay 'priority (+ vr--overlay-priority 2)))
	(progn
	  ;;(overlay-put overlay 'display nil)
	  (overlay-put overlay 'after-string 
		       (propertize (format " => %s" replacement) 'face (nth (mod i (length vr--match-faces)) vr--match-faces)))
	  (overlay-put overlay 'priority (+ vr--overlay-priority 0)))))))


(defun vr--get-replacements (feedback feedback-limit)
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
  (vr--feedback t) ;; only really needed when regexp has not been changed from default (=> no overlays have been created)
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
		(let ((replacement replacement))
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
  "Get interactive args for the vr/replace and vr/query-replace functions."
  (unwind-protect 
      (progn
	(let ((buffer-read-only t)) ;; make target buffer
	  (when vr--in-minibuffer (error "visual-regexp already in use."))
	  (setq vr--target-buffer (current-buffer))
	  (setq vr--target-buffer-start (if (and transient-mark-mode mark-active) 
						 (region-beginning)
					       (point)))
	  (setq vr--target-buffer-end (if (and transient-mark-mode mark-active) 
					       (region-end)
					     (point-max)))

	  (run-hooks 'vr/initialize-hook)
	  (setq vr--feedback-limit vr/default-feedback-limit)
	  
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
		vr--target-buffer-end)))
    (progn ;; execute on finish
      (setq vr--in-minibuffer nil)
      (vr--delete-overlay-displays)
      (vr--delete-overlays))))

(defun vr/replace (regexp replace start end)
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
	      vr--regexp-string regexp
	      vr--replace-string replace)
	;; do replacement
	(vr--do-replace))
    ;; execute on finish
    (setq vr--in-minibuffer nil)))

;; isearch starts here

(defun vr/isearch-forward ()
  "Like isearch-forward, but using Python (or custom) regular expressions."
  (interactive)
  (if t ;; todo engine
      (isearch-forward-regexp)
    (let ((isearch-search-fun-function 'vr/isearch-search-fun-function))
      (isearch-forward-regexp))))

(defun vr/isearch-backward ()
  "Like isearch-backward, but using Python (or custom) regular expressions."
  (interactive)
  (if t ;; todo engine
    (isearch-backward-regexp)  
    (let ((isearch-search-fun-function 'vr/isearch-search-fun-function))
      (isearch-backward-regexp))))

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
	 (regexp string ;; (if case-fold-search (concat "(?i)" string) string)
		 )
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

	(let ((matches-list (list))
	      (number-of-matches 0))
	  (setq message-line
		(let ((regexp-string regexp))
		  (vr--feedback-function
		   forward
		   (if count
		       count
		     ;; if no bound, the rest of the buffer is searched for the first match -> need only one match
		     (if bound nil 1))
		   (lambda (i j begin end)
		     (when (= j 0) (setq number-of-matches (1+ number-of-matches)))
		     (setq matches-list (cons (list i j begin end) matches-list))))))

	  ;; convert list to vector
	  (setq matches-vec (make-vector number-of-matches nil))
	  (let ((cur-match (list)))
	    (mapc (lambda (el)
		    (multiple-value-bind (i j begin end) el
		      (when (and (= j 0) (> i 0))
		      	(aset matches-vec (- i 1) (nreverse cur-match))
		      	(setq cur-match (list)))
		      (setq cur-match (cons end (cons begin cur-match)))))
		  (nreverse matches-list))
	    (when cur-match
	      (aset matches-vec (- (length matches-vec) 1) (nreverse cur-match)))))
	(when is-called-from-lazy-highlighting ;; store in cache
	  (setq vr--isearch-cache-key cache-key
		vr--isearch-cache-val matches-vec))))
    
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
C-w [not supported in visual-regexp],
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

(defun vr/query-replace (regexp replace start end)
  "Use vr/query-replace like you would use query-replace-regexp."
  (interactive 
   (vr--interactive-get-args))
  (unwind-protect 
      (progn 
	(when vr--in-minibuffer (error "visual-regexp already in use."))
	(setq vr--target-buffer (current-buffer)
	      vr--target-buffer-start start
	      vr--target-buffer-end end
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
	 ;;(search-function 'vr--isearch-forward) 
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

