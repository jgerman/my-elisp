(defun build-rove-single-test (pkg test-name)
  (let ((pkg-name (substring pkg 1)))
    (car (read-from-string (format "(rove:run-test '%s::%s)"
				   pkg-name
				   (symbol-name test-name))))))

(defun lisp-test-at-point ()
  (interactive)
  (let* ((pkg (slime-current-package))
	 (form (read-from-string (slime-defun-at-point)))
	 (def-symbol  (caar form))
	 (test-name   (cadar form)))
    (if (eq 'deftest def-symbol)
	(progn
	  (message (concat "Running test: " (symbol-name test-name)))
	  (slime-eval (build-rove-single-test pkg test-name)))
      (message "Not in test."))))
