# visual-regexp

visual-regexp is a package for emacs which enables you to use Python regular expressions and either a Python string or a Python expression for doing replacements.

While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches:

![entering regexp](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp0A.png)

While constructing the replacement in the minibuffer, you get live visual feedback for the replacements:

![entering replacement](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp0B.png)

It can be used to replace all matches in one go (like `replace-regexp`), or a decision can be made on each match (like `query-replace-regexp`). Incremental search using Python regular expressions is also supported, as a drop-in replacement for `isearch-forward-regexp`/`isearch-backward-regexp`. 

Besides doing replacements in regular buffers, one of the most interesting uses of visual-regexp is renaming a bunch of files directly in a dired buffer. See [Example 4](#example4).

## Installation

Put **visual-regexp.el** and **regexp.py** into the same directory. 

If you are using Emacs 24, you can get visual-regexp from [melpa](http://melpa.milkbox.net/) with the package manager.

You need a Python interpreter.
Add the following code to your init file. Of course you can select your own key bindings.

```Lisp
(add-to-list 'load-path "folder-in-which-visual-regexp-files-are-in/") ;; if the files are not already in the load path
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; to use visual-regexp isearch instead of the built-in regexp isearch (invoked by `C-M-s`, `C-M-r`), also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward)
(define-key esc-map (kbd "C-s") 'vr/isearch-forward)
```
To customize, use `M-x customize-group [RET] visual-regexp`. You can specify how the Python interpreter is invoked by modifying the `command-python` variable. The default is `python /path/to/regexp.py`.

## Examples

### Example 1
![Example 1](https://github.com/benma/visual-regexp.el/raw/master/screenshots/montage1.png)
### Example 2
Zero-width matches are indicated by a pipe. When replacing using a Python expression, the counter variable `i` is available.
![Example 2](https://github.com/benma/visual-regexp.el/raw/master/screenshots/montage2.png)
### Example 3
![Example 3](https://github.com/benma/visual-regexp.el/raw/master/screenshots/montage3.png)
### <a name="example4"/>Example 4
visual-regexp can be combined with `wdired-change-to-wdired-mode` to rename files directly in a dired buffer, using regular expressions with a live preview:
![Example 3](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp5B.png)