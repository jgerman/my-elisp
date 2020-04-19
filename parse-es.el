(require 'json)
(require 'seq)

(defun get-ids ()
  (let* ((js (json-read-from-string (buffer-substring-no-properties 1 (buffer-size)))))
    (seq-filter js)))
