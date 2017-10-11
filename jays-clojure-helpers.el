;;; jays-clojure-helpers.el --- Personal set of clojure helpers

;; Toggle Strings <-> Keywords
;;
;; Original code stolen from Jay Fields
;; http://blog.jayfields.com/2013/05/emacs-lisp-toggle-between-clojure.html
;;
;; Modified slightly.
;;
;; spaces become dashes when string->kw
;; dashes become spaces when kw->string

(defun char-at-point ()
  (interactive)
  (buffer-substring-no-properties (point) (+ 1 (point))))

(defun clj-string-name (s)
  (let* ((ss (substring s 1 -1)))
    (replace-regexp-in-string " " "-" ss)))

(defun clj-keyword-name (s)
  (let* ((ss (substring s 1)))
    (replace-regexp-in-string "-" " " ss)))

(defun delete-and-extract-sexp ()
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun toggle-clj-keyword-string ()
  (interactive)
  (save-excursion
    (if (equal 1 (point))
        (message "beginning of file reached, this was probably a mistake.")
      (cond ((equal "\"" (char-at-point))
             (insert ":" (clj-string-name (delete-and-extract-sexp))))
            ((equal ":" (char-at-point))
             (insert "\"" (clj-keyword-name (delete-and-extract-sexp)) "\""))
            (t (progn
                 (backward-char)
                 (toggle-clj-keyword-string)))))))

(defun basic-save-buffer (&optional called-interactively)
  "Save the current buffer in its visited file, if it has been modified.
The hooks `write-contents-functions' and `write-file-functions' get a chance
to do the job of saving; if they do not, then the buffer is saved in
the visited file in the usual way.
Before and after saving the buffer, this function runs
`before-save-hook' and `after-save-hook', respectively."
  (interactive '(called-interactively))
  (save-current-buffer
    ;; In an indirect buffer, save its base buffer instead.
    (if (buffer-base-buffer)
	(set-buffer (buffer-base-buffer)))
    (if (or (buffer-modified-p)
	    ;; handle the case when no modification has been made but
	    ;; the file disappeared since visited
	    (and buffer-file-name
		 (not (file-exists-p buffer-file-name))))
	(let ((recent-save (recent-auto-save-p))
	      setmodes)
          ;; If buffer has no file name, ask user for one.
	  (or buffer-file-name
              (let ((filename
                     (expand-file-name
                      (read-file-name "File to save in: "
                                      nil (expand-file-name (buffer-name))))))
                (if (file-exists-p filename)
                    (if (file-directory-p filename)
                        ;; Signal an error if the user specified the name of an
                        ;; existing directory.
                        (error "%s is a directory" filename)
                      (unless (y-or-n-p (format-message
                                         "File `%s' exists; overwrite? "
                                         filename))
                        (error "Canceled"))))
                (set-visited-file-name filename)))
	 
	  (save-restriction
	    (widen)
	    (save-excursion
	      (and (> (point-max) (point-min))
		   (not find-file-literally)
		   (/= (char-after (1- (point-max))) ?\n)
		   (not (and (eq selective-display t)
			     (= (char-after (1- (point-max))) ?\r)))
		   (or (eq require-final-newline t)
		       (eq require-final-newline 'visit-save)
		       (and require-final-newline
			    (y-or-n-p
			     (format "Buffer %s does not end in newline.  Add one? "
				     (buffer-name)))))
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n))))
	    ;; Support VC version backups.
	    (vc-before-save)
	    ;; Don't let errors prevent saving the buffer.
	    (with-demoted-errors (run-hooks 'before-save-hook))
	    (or (run-hook-with-args-until-success 'write-contents-functions)
		(run-hook-with-args-until-success 'local-write-file-hooks)
		(run-hook-with-args-until-success 'write-file-functions)
		;; If a hook returned t, file is already "written".
		;; Otherwise, write it the usual way now.
		(let ((dir (file-name-directory
			    (expand-file-name buffer-file-name))))
		  (unless (file-exists-p dir)
		    (if (y-or-n-p
			 (format-message
                          "Directory `%s' does not exist; create? " dir))
			(make-directory dir t)
		      (error "Canceled")))
		  (setq setmodes (basic-save-buffer-1))))
	    ;; Now we have saved the current buffer.  Let's make sure
	    ;; that buffer-file-coding-system is fixed to what
	    ;; actually used for saving by binding it locally.
	    (if save-buffer-coding-system
		(setq save-buffer-coding-system last-coding-system-used)
	      (setq buffer-file-coding-system last-coding-system-used))
	    (setq buffer-file-number
		  (nthcdr 10 (file-attributes buffer-file-name)))
	    (if setmodes
		(condition-case ()
		    (progn
		      (unless
			  (with-demoted-errors
			    (set-file-modes buffer-file-name (car setmodes)))
			(set-file-extended-attributes buffer-file-name
						      (nth 1 setmodes))))
		  (error nil))))
	  ;; If the auto-save file was recent before this command,
	  ;; delete it now.
	  (delete-auto-save-file-if-necessary recent-save)
	  ;; Support VC `implicit' locking.
	  (vc-after-save)
	  (run-hooks 'after-save-hook))
      (or noninteractive
          (not called-interactively)
          (files--message "(No changes need to be saved)")))))

(provide 'jays-clojure-helpers)
