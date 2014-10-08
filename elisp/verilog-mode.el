;;; Verilog-mode  -  Major mode for editing verilog source in emacs.

;;; Copyright (C) 1993 by Michael McNamara (mac@chronologic.com) Based
;;; on ideas in the Pascal mode, which is Copyright (C) 1993 by Espen
;;; Skoglund (espensk@stud.cs.uit.no)

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; INSTALLATION
;;; ============
;;; Put the verilog-mode.el file in your load-path and optionally byte
;;; compile it. If you don't know the load path of your emacs, try:
;;;   C-h v load-path
;;; If you don't have write access anywhere in this path, you can add
;;; this line to your .emacs file:
;;;
;;; (setq load-path (cons ("/users/me/lisp" load-path)))
;;;
;;; If you want to customize the verilog mode in your startup file, you
;;; can add these lines to your .emacs file (and remove the ;s at the
;;; beginning of the line):
;;;
;;; ;;; Verilog-mode customization.
;;; (autoload 'verilog-mode "verilog-mode" nil t)
;;; (setq auto-mode-alist (append (list (cons "\\.v$" 'verilog-mode)
;;;                                     (cons "\\.f$" 'verilog-mode))
;;;                       auto-mode-alist))
;;; (setq verilog-mode-hook '(lambda ()
;;;	                      ;; User specifications
;;;	                      (setq verilog-tab-always-indent t
;;;		                   verilog-auto-newline nil
;;;		                   verilog-auto-endcomments t
;;;		                   verilog-indent-level 3
;;;                                verilog-continued-expr 1
;;;		                   verilog-label-offset -2
;;;		                   verilog-case-offset 2
;;;                            )))

;;; USAGE
;;; =====
;;; If you have modified your startup file as described above, emacs
;;; should enter verilog-mode when you load a verilog source into emacs.
;;; If not, you will have to start verilog-mode manually:
;;;    M-x load-library verilog-mode
;;;    M-x verilog-mode
;;; When you have entered verilog-mode, you may get more info by pressing
;;; C-h m. You may also get online help describing various functions by:
;;;   C-h d <Name of function you want described>

;;; KNOWN BUGS / BUGREPORTS
;;; =======================
;;; Well, it's slow on large modules. Part of the problem is that
;;; Verilog isn't a single token block language (I.E., begin instead
;;; of {
;;; I've thought of caching markers, and updataing them when text it
;;; input, but, I got no time... Please, send enhancements

;;; LCD Archive Entry:
;;; verilog-mode|Mike McNamara|mac@chronologic.com|
;;; Major mode for editing Verilog code|
;;; 10-Oct-93|$Revision: 1.0 $|~/modes/verilog-mode.el.Z|

(defconst verilog-mode-version "1.0"
  "Version of this verilog mode.")

(defvar verilog-mode-abbrev-table nil
  "Abbrev table in use in Verilog-mode buffers.")
(define-abbrev-table 'verilog-mode-abbrev-table ())

(defvar verilog-mode-map ()
  "Keymap used in Verilog mode.")
(if (null verilog-mode-map)
    (setq verilog-mode-map (make-sparse-keymap)))

(define-key verilog-mode-map ";" 'electric-verilog-semi)
(define-key verilog-mode-map "." 'electric-verilog-dot)
(define-key verilog-mode-map ":" 'electric-verilog-colon)
(define-key verilog-mode-map "=" 'electric-verilog-equal)
(define-key verilog-mode-map "\r" 'electric-verilog-terminate-line)
(define-key verilog-mode-map "\t" 'electric-verilog-tab)
(define-key verilog-mode-map "\177" 'backward-delete-char-untabify)
(define-key verilog-mode-map "\C-ch" 'verilog-mark-function)
(define-key verilog-mode-map "\C-cb" 'verilog-insert-block)
(define-key verilog-mode-map "\M-*" 'verilog-star-comment)
(define-key verilog-mode-map "\C-c\C-c" 'verilog-comment-area)
(define-key verilog-mode-map "\C-c\C-u" 'verilog-uncomment-area)
(define-key verilog-mode-map "\C-ca" 'verilog-backward-to-beginning-of-function)
(define-key verilog-mode-map "\C-ce" 'verilog-forward-to-end-of-function)
(define-key verilog-mode-map "\C-cd" 'verilog-downcase-keywords)
(define-key verilog-mode-map "\C-cu" 'verilog-upcase-keywords)
(define-key verilog-mode-map "\C-cc" 'verilog-capitalize-keywords)

(defvar verilog-keywords
  '("always" "and" "assign" "begin" "buf" "bufif0" "bufif1"
    "case" "casex" "casez" "cmos" "deassign" "default" "defparm" "disable"
    "edge" "else" "end" "endcase" "endmodule" "endfunction" "endprimitive"
    "endspecify" "endtable" "endtask" "event" "for" "force" "forever"
    "fork" "function" "highz0" "highz1" "if" "initial" "inout" "input"
    "integer" "join" "large" "macromodule" "medium" "module" "nand"
    "negedge" "nmos" "nor" "not" "notif0" "notif1" "or" "output"
    "parameter" "pmos" "posedge" "primitive" "pull0" "pull1" "pullup"
    "rcmos" "real" "reg" "release" "repeat" "rnmos" "rpmos" "rtran"
    "rtranif0" "rtranif1" "scalared" "small" "specify" "specparam"
    "strength" "strong0" "strong1" "supply0" "supply1" "table"
    "task" "time" "tran" "tranif0" "tranif1" "tri" "tri0" "tri1"
    "triand" "trior" "trireg" "vectored" "wait" "wand" "weak0"
    "weak1" "while" "wire" "wor" "xnor" "xor"
    ))

(defvar v-m-s-t nil
  "Syntax table in use in Verilog-mode buffers.")
(setq v-m-s-t nil)
(setq v-m-s-t (make-syntax-table))
(modify-syntax-entry ?\\ "\\" v-m-s-t)
(modify-syntax-entry ?/ ". 142b" v-m-s-t)
(modify-syntax-entry ?* ". 23" v-m-s-t)
(modify-syntax-entry ?\n "> b"  v-m-s-t)
(modify-syntax-entry ?+ "." v-m-s-t)
(modify-syntax-entry ?- "." v-m-s-t)
(modify-syntax-entry ?= "." v-m-s-t)
(modify-syntax-entry ?% "." v-m-s-t)
(modify-syntax-entry ?< "." v-m-s-t)
(modify-syntax-entry ?> "." v-m-s-t)
(modify-syntax-entry ?( "." v-m-s-t)
(modify-syntax-entry ?) "." v-m-s-t)
(modify-syntax-entry ?& "." v-m-s-t)
(modify-syntax-entry ?| "." v-m-s-t)
(modify-syntax-entry ?_ "w" v-m-s-t)
(modify-syntax-entry ?\' "." v-m-s-t)

(defconst verilog-indent-synch 4
  "*Only go back this many sexp's when deciding indent of this block.")
(defconst verilog-indent-level 3
  "*Indentation of Verilog statements with respect to containing block.")

(defconst verilog-continued-expr 1
  "*Indentation of line that is a continued expression.")

(defconst verilog-label-offset -1
  "*Offset of Verilog label lines, case statements and record lines relative to
usual indentation.")

(defconst verilog-case-offset 2
  "*Indentation after case statements.")

(defconst verilog-vardecl-indent 15
  "*Indentation (from the beginning of line to `:' of the declaration.")

(defconst verilog-typedecl-indent 10
  "*Indentation (from the beginning of line to `=' of the declaration.")

(defconst verilog-auto-newline nil
  "*Non-nil means automatically newline after semicolons and the punctation mark
after an end.")

(defconst verilog-tab-always-indent t
  "*Non-nil means TAB in Verilog mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defconst verilog-auto-endcomments t
  "*Non-nil means a comment { ... } is set after the ends which ends cases and
functions. The name of the function or case will be set between the braces.")

(defun verilog-mode ()
  "Major mode for editing Verilog code.
Tab indents for Verilog code.
Delete converts tabs to spaces as it moves back.
\\{verilog-mode-map}
Variables controlling indentation style:
 verilog-tab-always-indent (default t)
    Non-nil means TAB in Verilog mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 verilog-auto-newline (default nil)
    Non-nil means automatically newline after semicolons and the various ends.
 verilog-auto-endcomments (default t)
    Non-nil means automatically set name of module, primitive or `case' 
    in a comment after the endmodule, endprimitive or endcase.
 verilog-indent-level (default 3)
    Indentation of Verilog statements within surrounding block.
 verilog-indent-synch (default 3)
    Number of previous Verilog statements to consider when indenting.
 verilog-continued-expr (default 1)
    Indentation of a line that is a continued expression.
 verilog-label-offset (default -1)
    Extra indentation for line that is a label, case statement or part of
    a record block.
 verilog-case-offset (default 2)
    Extra indent to the `:' in case statements.

When typing text, you should not worry about to get right indentions, they
will be set when you hit return. 

Turning on Verilog mode calls the value of the variable verilog-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map verilog-mode-map)
  (setq major-mode 'verilog-mode)
  (setq mode-name "Verilog")
  (setq local-abbrev-table verilog-mode-abbrev-table)
  (set-syntax-table v-m-s-t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'verilog-indent-line)
  (setq comment-indent-function 'verilog-indent-within-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (run-hooks 'verilog-mode-hook))

;;;
;;;  Electric functions
;;;
(defconst verilog-elec-end-regexp
 "end\\b\\|endcase\\b\\|endmodule\\b\\|endfunction\\b\\|endspecify\\b\\|endtable\\b\\|endtask\\b\\|endprimitive\\b\\|join\\b")
(defconst verilog-block-head-regexp
  "begin\\b\\|fork\\b\\|case\\b\\|casex\\b\\|casez\\b")
(defconst verilog-block-tail-regexp
  "end\\b\\|join\\b\\|endcase\\b")
(defconst verilog-zero-indent-regexp
  "module\\b\\|endmodule\\b\\|primitive\\b\\|endprimitive")
(defconst verilog-initialways-regexp
  "initial\\b\\|always\\b")
(defconst verilog-tfs-block-head-regexp
  "task\\b\\|function\\b\\|specify\\b\\|table\\b")
(defconst verilog-tfs-block-tail-regexp
  "endtask\\b\\|endfunction\\b\\|endspecify\\b\\|endtable\\b")
(defconst verilog-decl-regexp
  "\\<\\(inout\\|input\\|output\\|reg\\|real\\|integer\\|event\\|wire\\|wand\\|wor\\|tri\\|triand\\|trior\\|tri1\\|tri0\\|trireg\\|supply0\\|supply1)\\>")

(defun electric-verilog-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; If this line starts with a token that is electric,
  ;; indent accordingly
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at verilog-elec-end-regexp)
	(verilog-indent-line)))
  ;; Insert new line, and set up proper indentation
  (newline)
  (verilog-indent-line)
  ;; Maybe we should set some endcomments
  (if verilog-auto-endcomments
      (verilog-set-auto-comments))
  ;; Check if we shall indent inside comment
  (let ((setstar nil))
    (save-excursion
      (forward-line -1)
      (skip-chars-forward " \t")
      (cond ((looking-at "\\*[ \t]*)")
	     ;; Delete region between `*' and `)' if there is only whitespaces.
	     (forward-char 1)
	     (delete-whitespaces))
	    ((and (looking-at "(\\*\\|\\*[^)]")
		  (not (save-excursion
			 (search-forward "*)" (get-end-of-line) t))))
	     (setq setstar t))))
    ;; If last line was a star comment line then this one shall be too.
    (if setstar
	(progn
	  (insert "*")
	  (verilog-indent-command))
      (verilog-indent-line))))

(defun electric-verilog-semi ()
  "Insert ; character and correct line's indention."
  (interactive)
  (insert last-command-char)
  (save-excursion
    (beginning-of-line)
    (verilog-indent-line))
  (if verilog-auto-newline
      (electric-verilog-terminate-line)))

(defun electric-verilog-dot ()
  "Insert . character and correct line's indention."
  (interactive)
  (insert last-command-char)
  (save-excursion
    (beginning-of-line)
    (verilog-indent-line))
  (if verilog-auto-newline
      (electric-verilog-terminate-line)))

(defun electric-verilog-colon ()
  "Insert : and do all indentions except line indent on this line."
  (interactive)
  (insert last-command-char)
  ;; Do nothing of within string.
  (if (not (verilog-within-string))
      (progn
	(if (save-excursion
	      (backward-char 2)
	      (looking-at "[0-9]"))
	    (save-excursion
	      (beginning-of-line)
	      (verilog-indent-line)))
	(let ((verilog-tab-always-indent nil))
	  (verilog-indent-command)))))

(defun electric-verilog-equal ()
  "Insert = and do indention if within type declaration."
  (interactive)
  (insert last-command-char)
  (if (eq (nth 1 (verilog-calculate-indent t)) 'decl)
      (let ((verilog-tab-always-indent nil))
	(verilog-indent-command))))

(defun electric-verilog-tab ()
  "Function called when tab is pressed."
  (interactive)
  ;; Do nothing if within a string.
  (if (not (verilog-within-string))
      ;; If verilog-tab-always-indent is set then indent the beginning of
      ;; the line.
      (progn
	(if verilog-tab-always-indent
	    (save-excursion
	      (beginning-of-line)
	      (verilog-indent-line)))
	(verilog-indent-command))))

;;;
;;; Interactive functions
;;;
(defun verilog-insert-block ()
  "Insert begin ... end; block in the code with right indents."
  (interactive)
  (verilog-indent-line)
  (insert "begin")
  (electric-verilog-terminate-line)
  (save-excursion
    (electric-verilog-terminate-line)
    (insert "end")
    (beginning-of-line)
    (verilog-indent-line)))

(defun verilog-star-comment ()
  "Insert star comment in the code."
  (interactive)
  (verilog-indent-line)
  (insert "/*")
  (electric-verilog-terminate-line)
  (save-excursion
    (electric-verilog-terminate-line)
    (delete-whitespaces)
    (verilog-indent-line)
    (insert "*/"))
  (insert "* ")
  )

(defun verilog-downcase-keywords ()
  "Makes all verilog-keywords in the buffer lowercase."
  (interactive)
  (verilog-change-keywords 'downcase-word))

(defun verilog-upcase-keywords ()
  "Makes all verilog-keywords in the buffer uppercase."
  (interactive)
  (verilog-change-keywords 'upcase-word))

(defun verilog-capitalize-keywords ()
  "Makes all verilog-keywords in the buffer uppercase."
  (interactive)
  (verilog-change-keywords 'capitalize-word))

(defun verilog-change-keywords (change-word)
  "Change the keywords according to argument."
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward (mapconcat
			       'downcase verilog-keywords "\\>\\|\\<") nil t)
      (funcall change-word -1))))

;;;
;;; Other functions
;;;
(defun delete-whitespaces ()
  "Deletes the whitespaces around the current point."
  (interactive)
  (let ((pos (progn (skip-chars-backward " \t") (point))))
    (skip-chars-forward " \t")
    (delete-region pos (point))))

(defun get-beg-of-line ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun get-end-of-line ()
  (save-excursion
    (end-of-line)
    (point)))

(defun verilog-within-string ()
  "Return t if within string. Nil otherwise."
  (and (save-excursion (search-backward "\"" (get-beg-of-line) t))
       (save-excursion (not (search-backward "\"" (get-beg-of-line) t 2)))))

(defun verilog-check-if-within-comment ()
  "If within a comment, return the correct indent. Return nil otherwise."
  (let ((slash nil)
	(comstart (point))
	(comend (point)))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (looking-at "//")
	  (progn
	    (setq slash 't)
	    (setq comstart (point))
	    (end-of-line)
	    (setq comend (point))
	    )
	)
      )
    (if (not slash)
	(progn
	  (save-excursion
	    (if (re-search-backward "/\\*" nil t)
		(setq comstart (point))
	      (setq comstart 0)))
	  (save-excursion
	    (if (re-search-backward "\\*/" nil t)
		(setq comend (point))
	      (setq comend 0)))
	  )
      )
    (if (< comend comstart)
	(save-excursion
	  (goto-char comstart)
	  ;; Add 1 to indent if this is a starcomment
	  (if (looking-at "/\\*")
	      (1+ (current-column))
	    (current-column)))
      nil)))
(defun verilog-backward-sexp (&optional arg)
  "Version of `backward-sexp' that catches errors."
  (interactive "p")
  (or arg (setq arg 1))
  (condition-case nil
      (backward-sexp arg)
    (error (message "Note: mismatched parens")
	   (backward-word 1))))

(defun verilog-forward-sexp (&optional arg)
  "Version of `forward-sexp' that catches errors."
  (interactive "p")
  (or arg (setq arg 1))
  (condition-case nil
     (forward-sexp arg)
    (error (message "Note: mismatched parens")
	   (backward-word 1))))
  

(defun verilog-mark-function ()
  "Mark the current verilog function (or procedure), Leaving the mark at the
end of the function, and the point at the beginning."
  (interactive)
  (push-mark (point))
  (verilog-forward-to-end-of-function)
  (push-mark (point))
  (verilog-backward-to-beginning-of-function)
  ;;;(zmacs-activate-region)
  )

(defun verilog-comment-area (start end)
  "Put the current region in a comment. The comments that are in this area will
be changed so that `*)' becomes `!(*' and `}' becomes `!{'. These will
however be turned back to normal when the area is uncommented by pressing
\\[verilog-uncomment-area].
The commented area starts with: `ifdef COMMENTED_OUT , and ends with:
`endif. If these texts are changed, uncomment-area
will not be able to recognize them."
  (interactive "r")
  (save-excursion
    ;; Insert start and endcomments
    (goto-char end)
    (if (and (save-excursion (skip-chars-forward " \t") (eolp))
	     (not (save-excursion (skip-chars-backward " \t") (bolp))))
	(forward-line 1)
      (beginning-of-line))
    (insert "`endif /* COMMENTED_OUT */")
    (setq end (point))
    (newline)
    (goto-char start)
    (beginning-of-line)
    (insert "`ifdef COMMENTED_OUT")
    (newline)
    ))
(defun verilog-uncomment-area ()
  "Uncomment a commented area and change all deformed comments in this area
back to normal. This function does nothing if the pointer is not in a commented
area. See also verilog-comment-area."
  (interactive)
  (save-excursion
    (let ((start (point))
	  (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
	(setq start (progn (search-backward "`ifdef COMMENTED_OUT" nil t)
			   (point)))
	(setq end (progn (search-forward "`endif /* COMMENTED_OUT */" nil t)
			 (point))))
      ;; Check if we're really inside a comment
      (if (or (equal start (point)) (<= end (point)))
	  (message "Not standing within commented area.")
	(progn
	  ;; Remove endcomment
	  (goto-char end)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point))))
	  ;; Remove startcomment
	  (goto-char start)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point)))))))))

(defun verilog-backward-to-beginning-of-function ()
  "Move backwards to the beginning of this function or procedure."
  (interactive)
  ;; Check if this is a
  (if (save-excursion
	(re-search-backward "\\<end" nil t)
	(looking-at "end"))
      (beginning-of-buffer)
    (let ((nest-depth 0) (nest-max 0)
	  (nest-noexit 1))
      (beginning-of-line)
      ;; First we find the max depth of the nesting
      (save-excursion
	(while (not (or (bobp) (looking-at verilog-zero-indent-regexp)))
	  (verilog-backward-sexp 1)
	  (cond ((looking-at verilog-block-head-regexp)
		 (setq nest-depth (1+ nest-depth)))
		((looking-at "end\\b")
		 (setq nest-depth (1- nest-depth))))
	  (setq nest-max (max nest-depth nest-max))))
      ;; Then we can start searching
      (setq nest-depth 0)
      (while (not (or (bobp) (and (looking-at verilog-zero-indent-regexp)
				  (zerop nest-noexit))))
	(verilog-backward-sexp 1)
	(cond ((looking-at verilog-block-head-regexp)
	       (setq nest-depth (1+ nest-depth)))
	      ((looking-at "end\\(\\b\\|;\\|\\.\\)")
	       (if (equal nest-depth nest-max)
		   (setq nest-noexit (1+ nest-noexit)))
	       (setq nest-depth (1- nest-depth)))
	      ((looking-at verilog-zero-indent-regexp)
	       (setq nest-noexit (1- nest-noexit))))))))

(defun verilog-forward-to-end-of-function ()
  "Moves the point to the end of the function."
  (interactive)
  (if (not (looking-at verilog-zero-indent-regexp))
      (verilog-backward-to-beginning-of-function))
  (if (bobp)
      (end-of-buffer)
    (progn
      (let ((nest-depth 0)
	    (func-depth 1))
	(while (not (or (and (zerop nest-depth) (zerop func-depth)) (eobp)))
	  (verilog-forward-sexp 2)
	  (if (not (eobp))
	      (progn
		(verilog-backward-sexp 1) ; Move to the beginning of the next sexp
		(cond ((looking-at verilog-block-head-regexp)
		       (setq nest-depth (1+ nest-depth)))
		      ((looking-at "end\\(\\b\\|;\\|\\.\\)")
		       (setq nest-depth (1- nest-depth))
		       (if (zerop nest-depth)
			   (setq func-depth (1- func-depth))))
		      ((looking-at "function\\b\\|procedure\\b")
		       (setq func-depth (1+ func-depth)))))))
	(end-of-line)))))


(defun verilog-set-auto-comments ()
  "Set a { case } at the end of the line if there is an end on the line which
ends a case block. Set a { <name> } at the end of the line if there is an end
of the line which ends a function or procedure."
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (cond 
	((and (looking-at "endmodule\\b\\|endprimitive\\b\\|endcase\\b")
	     (not (save-excursion
		    (end-of-line)
		    (search-backward "*/" (get-beg-of-line) t))))
	 (let (endtoken)
	   (cond 
	    ((looking-at "endmodule\\b")
	     (setq endtoken "module"))
	    ((looking-at "endprimitive\\b")
	     (setq endtoken "primitive"))
	    ((looking-at "endcase\\b")
	     (setq endtoken "case"))
	    )
	   ;; Find the module name
	   (save-excursion
	     (while (not (or (looking-at endtoken)
			     (bobp)))
	       (verilog-backward-sexp 1)
	       )
	   (let ((last-command nil))
	     ;; Find the function name and put it in braces
	     (save-excursion
	       (skip-chars-forward "^ \t")
	       (skip-chars-forward " \t")
	       (copy-region-as-kill (point)
				    (save-excursion
				      (skip-chars-forward "a-zA-Z0-9_")
				      (point))))))
	 (end-of-line)
	 (delete-whitespaces)
	 (insert " /* ")
	 ;; We've filled up the kill ring, but hey, who cares?
	 (yank) (rotate-yank-pointer 1)
	 (insert " */"))))))



;;;
;;; Indent functions and calculation of indent
;;;
(defun verilog-indent-command ()
  "Indent current line as Verilog code and/or indent within line."
  ;; Call verilog-indent-line. This does nothing if we're not at the
  ;; beginning of the line.
  (verilog-indent-line)
  (let ((indent (verilog-calculate-indent t))
	(pos 0))
    (save-excursion
      (cond ((or (eq (nth 1 indent) 'case)
		 (eq (nth 1 indent) 'record))
	     ;; Indent for case and record blocks
	     (beginning-of-line)
	     (if (search-forward ":" (get-end-of-line) t)
		 (progn
		   ;; Indent before colon
		   (backward-char 1)
		   (delete-whitespaces)
		   (indent-to (max (find-leading-case-colon)
				   (1+ (current-column))))
		   ;; Indent after colon
		   (forward-char 1)
		   (delete-whitespaces)
		   (indent-to (1+ (current-column))))
	       ;; Indent if there is no colon
	       (progn
		 (beginning-of-line)
		 (skip-chars-forward " \t")
		 (if (not (eolp))
		     (progn
		       (skip-chars-forward "0-9a-zA-Z\"\'_;")
		       (delete-whitespaces)
		       (indent-to (max (find-leading-case-colon)
				       (1+ (current-column)))))))))
	    ((eq (nth 1 indent) 'decl)
	     ;; Indent for declarations
	     (let ((posii (get-beg-of-line)))
	       (re-search-backward verilog-decl-regexp nil t)
	       (cond ((looking-at "var\\b")
		      (declindent-middle-of-line ":" posii
						 verilog-vardecl-indent))
		     ((looking-at "type\\b\\|const\\b")
		      (declindent-middle-of-line "=" posii
						 verilog-typedecl-indent)))))

	    ((eq (nth 1 indent) 'module)
	     ;; Indent for parameterlist
	     ;; Done twice in case something has changed
	     (verilog-indent-parameter-list)
	     (verilog-indent-parameter-list))))
    ;; Go to the end of a line if rest of line contains only whitespaces
    (if (save-excursion (skip-chars-forward " \t") (eolp))
	(end-of-line))))

(defun verilog-indent-line ()
  "Indent current line as Verilog code."
  (let ((indent (list 0 nil))
	(comindent 0)
	beg (point))
    (save-excursion
      (beginning-of-line)
      (setq indent (verilog-calculate-indent)))
    ;; If we are inside a comment, do special indent.
    (if (setq comindent (verilog-check-if-within-comment))
	(indent-within-comment comindent)
      ;; Skip the rest if we're not standing on the beginning of a line.
      (if (save-excursion (skip-chars-backward " \t") (bolp))
	  (progn
	    (beginning-of-line)
	    (delete-whitespaces)
	    ;; When to skip the extra indent:
	    ;; If we are standing at end or until.
	    ;; If we are in an if statement and standing at else,
	    ;;  begin or repeat
	    ;; If we are in a inital or always statement and standing
	    ;;  at begin or end.
	    (cond ((or (or (looking-at verilog-block-tail-regexp)
			   (looking-at verilog-tfs-block-tail-regexp)
			   (not (nth 1 indent)))

		       (and (eq (nth 1 indent) 'if)
			    (looking-at "begin\\b\\|else\\b"))

		       (and (eq (nth 1 indent) 'initialways)
			    (looking-at "begin\\b\\|\\repeat\\b"))

		       (and (eq (nth 1 indent) 'zir)
			    (looking-at verilog-zero-indent-regexp))

		       (and (eq (nth 1 indent) 'tfs)
			    (looking-at verilog-tfs-block-head-regexp))
		       )
		   (indent-to (car indent)))

		  ;; Certain sections are always one indent level
		  ((and (eq (nth 1 indent) 'oir)
			(looking-at verilog-initialways-regexp))
		   (indent-to (+ (car indent) verilog-indent-level)))

		  ;; Continued expression
		  ((eq (nth 1 indent) 'contexp)
		   (indent-to (+ (car indent) verilog-continued-expr)))

		  ;; If this is a part of a case or record block,
		  ;; then modify the indent level.
		  ((or (eq (nth 1 indent) 'case)
		       (eq (nth 1 indent) 'record))
		   (indent-to (+ (car indent) verilog-indent-level
				 verilog-label-offset)))

		  ;; If this is a label - don't indent.
		  ((looking-at "[0-9]*:")
		   (skip-chars-forward "0-9:")
		   (delete-whitespaces)
		   (indent-to (+ (car indent) verilog-indent-level)))

		  ;; If this is inisde a parameter list, do special indent
		  ((eq (nth 1 indent) 'module)
		   (verilog-indent-parameter-list))

		  ;; All other indents are set normaly.
		  (t
		   (indent-to (+ (car indent) verilog-indent-level)))))))))

(defun verilog-calculate-indent (&optional arg)
  "Search backward in code to find the right indent level.
Return a list containing:
1. Indent level
2. The indent keyword (begin, case etc.), or nil if backtracking failed.
If arg is non-nil, we do not search for continued expressions."
  (let ((verilog-nest-depth 1)
;;	(num_sexps 0)
	(oldpos (save-excursion (forward-line -1) (end-of-line) (point)))
	(samepos (point)) (if-is-set t)
	(return-struct (list 0 nil)) (pos 0)
	(contexpr nil) (after-contexpr (not arg))
	(case-fold-search t))

    (save-excursion
      ;; Search back until indent level 0 is found
      (while (and (not (zerop verilog-nest-depth))
		  (not (bobp))
;;		  (not (eq verilog-indent-synch num_sexps))
		  )
	(progn
	  ;; Try to jump back across blocks until indent level changes
	  (if (looking-at verilog-elec-end-regexp)
	      (progn
		(cond
		      ((looking-at "end\\b")
		       (re-search-backward "begin\\b\\|end\\b" nil t))
		      ((looking-at "join\\b")
		       (re-search-backward "fork\\b\\|join\\b" nil t))
		      ((looking-at "endcase\\b")
		       (re-search-backward "case\\b\\|casex\\b\\|casez\\b" nil t))
		      ((looking-at "endmodule\\b")
		       (re-search-backward "module\\b" nil t))
		      ((looking-at "endfunction\\b")
		       (re-search-backward "function\\b" nil t))
		      ((looking-at "endspecify\\b")
		       (re-search-backward "specify\\b" nil t))
		      ((looking-at "endtable\\b")
		       (re-search-backward "table\\b" nil t))
		      ((looking-at "endtask\\b")
		       (re-search-backward "task\\b" nil t))		      
		      ((looking-at "endprimitive\\b")
		       (re-search-backward "primitive\\b" nil t))
		      ((looking-at "else\\b")
		       (re-search-backward "if\\b\\|else\\b" nil t))		       
		      (t
;;		       (setq num_sexps (1+ num_sexps))
		       (verilog-backward-sexp 1))
		      ))
;;	    (setq num_sexps (1+ num_sexps))
	    (verilog-backward-sexp 1))
	  ;; Figure out if we are looking at
	  ;; complete expression, or a continued exp.
	  (if (save-excursion
		(setq pos (point))
		(end-of-line)
		(search-backward ";" pos t))
	      (setq if-is-set nil
		    after-contexpr nil))
	  ;; Some tokens don't end in ;
	  (if (looking-at verilog-elec-end-regexp)
	      (setq after-contexpr nil))

	  ;; Ok, figure the indent level of this exp.
	  (cond
		;; tokens that end an indented block
		((looking-at verilog-block-tail-regexp)
		 (setq if-is-set nil)
		 (if after-contexpr
		     (setq verilog-nest-depth 0
			   contexpr t)
		   (setq verilog-nest-depth (1+ verilog-nest-depth))))

	        ;; tokens that start an indented block
	        ((looking-at verilog-block-head-regexp)
		 (setq verilog-nest-depth (1- verilog-nest-depth)))

		;; special continued expressions
		((and (looking-at "if\\b\\|else\\b\\|while\\b\\|for\\b")
		      if-is-set)
		 (setq verilog-nest-depth 0))

		;; tokens that start a level 1 block
		((looking-at verilog-tfs-block-head-regexp)
		 (setq verilog-nest-depth (1- verilog-nest-depth)))

		;; tokens that end a level 1 block
		((looking-at verilog-tfs-block-tail-regexp)
		 (setq verilog-nest-depth (1+ verilog-nest-depth)))

		;; tokens that start a level 0 block
		((looking-at verilog-zero-indent-regexp)
		 (setq verilog-nest-depth (1- verilog-nest-depth)))

		;;
		;; other continued expressions
		(after-contexpr
		 (save-excursion
		   ;; First, we have to be at the begining of a line
		   (if (and (progn (skip-chars-backward " \t") (bolp))
			    ;; Blank lines don't count
			    (not (progn (skip-chars-forward " \t") (eolp)))
			    ;; But nonblank without ';' do
			    (not (search-forward ";" (get-end-of-line) t)))
		       (save-excursion
			 (forward-line -1)
			 (end-of-line)
			 (verilog-backward-sexp 1)
			 (if (or (looking-at "\\(begin\\|repeat\\|else\\|end\\)")
				 (progn
				   (skip-chars-forward "^; " (get-end-of-line))
				   (equal (char-to-string (following-char))
					  ";")))
			     (setq verilog-nest-depth 0))
			 (setq contexpr t)))))
		)))

      (cond (contexpr
	     (setq return-struct (list (verilog-lstart-col) 'contexp)))

	    ((looking-at "\\(begin\\b\\|fork\\b\\)")
	     (setq return-struct (list (verilog-lstart-col) 'begin)))

	    ((looking-at verilog-zero-indent-regexp)
	     (setq return-struct (list 0 'zir)))

	    ((looking-at verilog-tfs-block-head-regexp)
	     (setq return-struct (list verilog-indent-level 'tfs)))

	    ((looking-at "else\\b")
	     (setq return-struct (list (save-excursion
					 (re-search-backward "if\\b" nil t)
					 (verilog-lstart-col)) 'contexp))
	     ;; Indent line in case this is a multiple if
	     (beginning-of-line)
	     (delete-whitespaces)
	     (indent-to (car return-struct)))
	    ((looking-at "if\\b")
	     (if (save-excursion
		   (narrow-to-region (get-beg-of-line) (point))
		   (verilog-backward-sexp 1)
		   (widen)
		   (looking-at "else\\b"))
		 ;; Indent line if this is a multiple if
		 (progn
		   (beginning-of-line)
		   (delete-whitespaces)
		   (indent-to (save-excursion
				(re-search-backward "if\\b" nil t)
				(verilog-lstart-col)))))
	     ;; This could be a continued expression
	     (if (and after-contexpr
		      (not (save-excursion (re-search-forward
					    "begin\\b" (get-end-of-line) t))))
		 (setq return-struct (list (verilog-lstart-col) 'contexp))
	       (setq return-struct (list (verilog-lstart-col) 'if))))

	    ((looking-at "repeat\\b")
	     (setq return-struct (list (verilog-lstart-col) 'repeat)))

	    ((looking-at "case\\b\\|casex\\b\\|casez\\b")
	     (setq return-struct (list (current-column) 'case)))

;;	    ((looking-at verilog-initialways-regexp)
;;	     (progn
;;	       (message "oir")
;;	       (setq return-struct (list 0 'oir))))
;;
	    ((looking-at verilog-initialways-regexp)
	     ;; This could ba a continued expression
	     (if (and after-contexpr
		      (not (save-excursion (re-search-forward
					    "begin\\b" (get-end-of-line) t))))
		 (setq return-struct (list (verilog-lstart-col) 'contexp))
	       (setq return-struct (list (current-column) 'initialways))))

	    ((looking-at "task\\b\\|function\\b")
	     ;; Make sure that this is a function with parameters, and
	     ;; that we are actually standing inside the paranthesis.
	     (let ((spos (save-excursion
			   (search-forward "(" samepos t) (point)))
		   (epos (save-excursion
			   (search-forward ")" samepos t) (point))))
	       (if (and (>= samepos spos) (or (< samepos epos)
					      (> spos epos)))
		   (setq return-struct (list 0 'function))
		 (setq return-struct (list 0 nil)))))
	    ((looking-at "var\\b\\|label\\b\\|const\\b\\|type\\b")
	     ;; Are we really in the declaration part?(Check for blank lines)
	     (if (< oldpos (point))
		 (setq return-struct (list 0 'decl))
	       (if (save-excursion
		     (not (re-search-forward "^[ \t]*$" oldpos t)))
		   (setq return-struct (list 0 'decl))
		 (setq return-struct (list 0 nil)))))
	    (t
	     (setq return-struct (list 0 nil))))
      return-struct)))

(defun verilog-lstart-col ()
  "Return the column of the beginning of the first command on the line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward ":0-9")
    (skip-chars-forward " \t")
    (current-column)))


(defun indent-within-comment (indent)
  "Indent comments and/or indent text within comment."
  (progn
    ;; If we are at the beginning of the line, then we indent this line.
    (if (save-excursion (skip-chars-backward " \t") (bolp))
	(progn
	  (beginning-of-line)
	  (delete-whitespaces)
	  (indent-to indent))
      ;; Do nothing if we're not in a star comment.
      (if (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (looking-at "\\*\\|(\\*"))
	  (save-excursion
	    (beginning-of-line)
	    (search-forward "*")
	    (delete-whitespaces)
	    (indent-to (+ (current-column) 2)))))))

(defun find-leading-case-colon ()
  "Return the column position of the first colon after the case-of or record
line, or the place it should be if no such line exists."
  (let ((pos (save-excursion
	       (beginning-of-line)
	       (skip-chars-forward " \t")
	       (point))))
    (save-excursion
      (re-search-backward "\\<\\(case\\|record\\)\\>")
      (forward-line 1)
      (skip-chars-forward " \t")
      (if (not (eq pos (point)))
	  (progn
	    (search-forward ":" (get-end-of-line) t)
	    (1- (current-column)))
	(+ (current-column) verilog-case-offset)))))


(defun verilog-indent-parameter-list ()
  "Indent this line as part of a parameter list in a function."
  )
;;;  (let ((indents (get-highest-indents-in-parameterlist))
;;;	(pos 0))
;;;    (if (not (progn (beginning-of-line)
;;;		    (search-forward "(" (get-end-of-line) t)))
;;;	(progn (beginning-of-line)
;;;  	       (skip-chars-forward " \t")))
;;;    ;; Indent region in front of var
;;;    (skip-chars-forward " \t")
;;;    (delete-whitespaces)
;;;    (indent-to (nth 0 indents))
;;;    (if (looking-at "var\\b")
;;;	(forward-char 3))
;;;    ;; Indent parameternames
;;;    (delete-whitespaces)
;;;    (indent-to (nth 1 indents))
;;;    (if (not (save-excursion (skip-chars-forward " \t") (eolp)))
;;;	(progn
;;;	  ;; Indent colon
;;;	  (if (search-forward ":" (get-end-of-line) t)
;;;	      (backward-char 1)
;;;	    (end-of-line))
;;;	  (delete-whitespaces)
;;;	  (indent-to (nth 2 indents))
;;;	  ;; Indent after colon
;;;	  (if (equal (following-char) ?:)
;;;	      (progn
;;;		(forward-char 1)
;;;		(delete-whitespaces)
;;;		(indent-to (+ 2 (nth 2 indents)))))))))

;;;(defun get-highest-indents-in-parameterlist ()
;;;  "To get the indents to use in a parameterlist. Returns:
;;;1. Indent to the beginning of the line.
;;;2. Indent to the beginning of the parameter names.
;;;3. Indent to the right colon position."
;;;  (save-excursion
;;;    (let ((start (progn
;;;		   (re-search-backward
;;;		    "\\<\\(function\\|module\\)\\>" nil t)
;;;		   (search-forward "(")
;;;		   (current-column)))
;;;	  (arglength 0) (vardecl nil) (done nil))
;;;      (while (not (or done (eobp)))
;;;	(beginning-of-line)
;;;	(if (save-excursion
;;;	      (re-search-forward "\\<var\\>" (get-end-of-line) t))
;;;	      (setq vardecl t))
;;;	(if (not (re-search-forward ":" (get-end-of-line) t))
;;;	    (setq done t))
;;;	(skip-chars-backward ": \t")
;;;	(setq arglength (max arglength (current-column)))
;;;	(forward-line 1))
;;;      (if vardecl
;;;	  (list start (+ start 4) (1+ arglength))
;;;	(list start start (1+ arglength))))))

(defun declindent-middle-of-line (declkey endpos defaultindent)
  "Indent declaration line."
  )
;;;  (let ((decindent 0))
;;;    (if (search-forward declkey endpos t)
;;;	(setq decindent (1- (current-column)))
;;;      (setq decindent defaultindent))
;;;    (goto-char endpos)
;;;    (end-of-line)
;;;    (if (save-excursion (search-backward declkey endpos t))
;;;	(progn (search-backward declkey) (skip-chars-backward " \t"))
;;;      (skip-chars-backward " \t"))
;;;    (delete-whitespaces)
;;;    (indent-to (max decindent (1+ (current-column))))
;;;    ;; Indent after `declkey'
;;;    (if (looking-at declkey)
;;;	(progn
;;;	  (forward-char 1)
;;;	  (delete-whitespaces)
;;;	  (indent-to (1+ (current-column)))))))

