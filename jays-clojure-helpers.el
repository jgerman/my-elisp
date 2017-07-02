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


(provide 'jays-clojure-helpers)
