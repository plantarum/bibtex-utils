;;; bibtex-utils.el --- Provides utilities for extending BibTeX mode

;; Copyright (C) 2007 Bastien Guerry
;; Copyright (C) 2010-11 Matt Lundin
;; Copyright (C) 2014 Tyler Smith
;;
;; Original Author: bzg AT altern DOT org
;; Author: Tyler Smith <tyler@plantarum.ca>
;; Version: 0.3
;; Keywords: bibtex
;; URL: https://github.com/plantarum/bibtex-utils

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; 
;; This file is not part of GNU Emacs.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'bibtex-utils)

;; Bind bu-make-field-keywords to a convenient key:
;;   (define-key bibtex-mode-map "\C-ck" 'bu-make-field-keywords)

;; This package also provides a minor mode, bibtex-search-minor-mode, which
;; is enabled in the search results buffer. At the moment this just
;; provides a few convenient keybindings: bury the buffer with 'b', or kill
;; it with 'k' or 'q'. You can add additional bindings with:
;; (define-key 'bibtex-search-minor-mode-map "j" 'your-function)
;;
;;; Code:
(require 'bibtex)
(require 'reftex)

;;;;;;;;;;;;;;
;; Keywords ;;
;;;;;;;;;;;;;;

(defvar bu-keywords-values)
(make-variable-buffer-local 'bu-keywords-values)
(make-obsolete-variable
 'bibtex-keywords-values
 "Bibtex-utils functions and variables are now prefixed with bu-, not bibtex-"
 "06 July 2014")

(defun bu-parse-keywords-values ()
  (setq bu-keywords-values (bu-collect-keywords-values)))

(add-hook 'bibtex-mode-hook 'bu-parse-keywords-values)

(defun bu-collect-keywords-values (&optional regexp)
  "Collect values in keywords fields of all BibTeX entries.
Maybe restrict the values to those matching REGEXP. Keywords may be phrases
separated by commas. Multiple spaces or newlines within a keyword will be
removed before collection."
  (save-excursion
    (goto-char (point-min))
    (let (keywords kstring)
      (while (re-search-forward "^\\s-*keywords.*{\\([^}]+\\)}" nil t)
        ;; TWS - remove newlines/multiple spaces:
	(setq kstring (replace-regexp-in-string "[ \t\n]+" " " (match-string-no-properties 1)))
	(mapc
	 (lambda (v) 
	   (if regexp (if (string-match regexp v) 
			  (add-to-list 'keywords v t))
	     (add-to-list 'keywords v t)))
          (split-string kstring ",[ \n]*\\|{\\|}" t)))
      keywords)))

;;; bibtex-select-entries is now deprecated in favour of
;;; bibtex-search-entries, provided in bibtex.el

(defun bu-make-field-keywords (&optional arg)
  "Make a keywords field.
If ARG is nil, ask for each keyword and offer completion over
keywords that are already available in the buffer.  Inserting 
the empty string will quit the prompt. If the keyword is not already
present in the buffer, it will be added to the local variable
bu-keywords-values. Note that if you use ido-ubiquitous, the value of
  `ido-ubiquitous-enable-old-style-default' is temporarily set to t within
the body of this command."
  (interactive "P")
  (let ((elist (save-excursion (bibtex-beginning-of-entry)
			       (bibtex-parse-entry)))
        (ido-ubiquitous-enable-old-style-default t)
	append)
    (if (assoc "keywords" elist)
	(progn (setq append t)
	       (bibtex-beginning-of-entry)
	       (goto-char 
		(car (last (or (bibtex-search-forward-field "keywords" t)
                               (progn (setq append nil)
                                      (bibtex-search-forward-field "OPTkeywords" t)))))))
      (bibtex-make-field "keywords" t nil))
    (skip-chars-backward "}")
    (unless arg
      (let ((cnt 0)
            k)
	(while (and (setq k (completing-read 
                             "Keyword (RET to quit): " bu-keywords-values nil))
		    (not (equal k "")))
	  (when append (insert ", ")
                (setq append nil))
	  (setq cnt (1+ cnt))
	  (insert (format "%s%s" (if (> cnt 1) ", " "") k))
          (add-to-list 'bu-keywords-values k))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump to pdfs, html ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom bu-pdf-dir "~/pdfs/"
  "The location of pdf files for bibtex-utils"
  :group 'bibtex-utils)

(defcustom bu-pdf-viewer nil
  "Pdf viewer of choice for opening documents from bibtex-utils.
When set to nil, the pdf is opened in emacs using whichever mode is 
is configured - pdf-tools is recommended."
  :group 'bibtex-utils)

(defcustom bu-doi-resolver "/usr/bin/firefox"
  "Browser to use to look-up doi references from bibtex files"
  :group 'bibtex-utils)

(defcustom bu-doi-prefix "doi:"
  "string to prepend to a doi reference when passing it to
`bu-doi-resolver'
For firefox with the DOI resolver extension, it should be 'doi:'
For chromium with the DOI resolver extension, it should be 'doi '.
Note that chromium doesn't really work with this yet."
  :group 'bibtex-utils)

(defun bu-open-doc ()
    "Open the document for the current bibtex entry.
First tries to open a pdf based on the entry key. Failing that, it will
check for a doi, and finally a url. Assumes the pdf has the same name
as the bibtex key, and is present in `bu-pdf-dir'. See also `bu-doi-prefix',
`bu-doi-resolver', `bu-pdf-viewer'"
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bpe (bibtex-parse-entry))
           (file-name (concat bu-pdf-dir
                              (cdr (assoc "=key=" bpe))
                              ".pdf"))
           (doi (assoc "doi" bpe))
           (url (or (assoc "url" bpe)
                    (assoc "URL" bpe))))
      (cond ((file-exists-p file-name)
             (if bu-pdf-viewer
                 (async-shell-command (concat bu-pdf-viewer " " file-name))
               (find-file file-name)))
            (doi
             (async-shell-command
              (concat bu-doi-resolver " " bu-doi-prefix
                      (replace-regexp-in-string "{\\|}\\|\"" ""  (cdr doi)))))
            (url
             (browse-url (replace-regexp-in-string "{\\|}\\|\"" ""  (cdr url))))
            (t (message "File doesn't exist, and no doi present!"))))))

(defun bu-jump-to-doc ()
  "Open the document associated with the bibtex citation at point."
  (interactive)
  (save-excursion
    (let ((state (current-window-configuration)))
      (if (reftex-pop-to-bibtex-entry (reftex-this-word "^{}%\n\r, \t")
                                      (condition-case nil
                                          (reftex-get-bibfile-list)
                                        (error reftex-default-bibliography)))
          (bu-open-doc)
        (set-window-configuration state)
        (message "No .bib file available!")))))

;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

(defun bu-previous-entry ()
  "Move to the beginning of the entry, or the previous entry.
If at the beginning of the current entry, move back one entry.
Otherwise, move to the beginning of the current entry."
  (interactive)
  (re-search-backward "^[ \t]*@" nil 'move))

(defun bu-next-entry ()
  "Move to the beginning of the next entry."
  (interactive)
  (if (looking-at "^[ \t]*@")
      (forward-char))
  (re-search-forward "^[ \t]*@" nil 'move)
  (beginning-of-line)
  (if (> (point) 
         (save-excursion
           (goto-char (window-end))
           (forward-line -2)
           (point)))
      (recenter 2)))

(define-key bibtex-mode-map (kbd "M-n") 'bu-next-entry)
(define-key bibtex-mode-map (kbd "M-p") 'bu-previous-entry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibtex Search Results Buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode bibtex-search-minor-mode
  "A minor mode for manipulating the results of bibtex search."
  nil nil
  '(("b" . bury-buffer)
    ("k" . #'(lambda () (interactive) (kill-buffer nil)))
    ("q" . #'(lambda () (interactive) (kill-buffer nil)))
    ("n" . #'(lambda () (interactive) (bu-next-entry)))
    ("p" . #'(lambda () (interactive) (bu-previous-entry)))))

(defun bibtex-search-mode-check ()
  "Checks if the current buffer is the bibtex-search-buffer, and if so,
turns on bibtex-minor-mode to provide a few useful keybindings."
  (if (string-equal (buffer-name) bibtex-search-buffer)
      (bibtex-search-minor-mode)))

(add-hook 'bibtex-mode-hook 'bibtex-search-mode-check)

(provide 'bibtex-utils)

