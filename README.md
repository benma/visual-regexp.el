# visual-regexp

visual-regexp for Emacs is like `replace-regexp`, but with interactive visual feedback directly in the buffer.

While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches:

![entering regexp](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp0A.png)

While constructing the replacement in the minibuffer, you get live visual feedback for the replacements:

![entering replacement](https://github.com/benma/visual-regexp.el/raw/master/screenshots/visual-regexp0B.png)

It can be used to replace all matches in one go (like `replace-regexp`), or a decision can be made on each match (like `query-replace-regexp`). 

Besides doing replacements in regular buffers, one of the most interesting uses of visual-regexp is renaming a bunch of files directly in a dired buffer. See [Example 4](#example4).

## Installation

If you are using Emacs 24, you can get visual-regexp from [melpa](http://melpa.milkbox.net/) with the package manager.

Add the following code to your init file. Of course you can select your own key bindings.

```Lisp
(add-to-list 'load-path "folder-in-which-visual-regexp-files-are-in/") ;; if the files are not already in the load path
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
```
To customize, use `M-x customize-group [RET] visual-regexp`. 

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