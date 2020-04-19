(defun foo ()
  (message "Hello, world!!"))

(foo)

(defun bar ()
  (interactive)
  (buffer-list ))

(defun kill-all-cider-buffers ()
  (interactive)
  (dolist (b (buffer-list))
    (when (string-prefix-p "*cider-repl" (buffer-name b))
      (kill-buffer b))))


(defun get-pods ()
  (interactive)
  (split-string (shell-command-to-string "kubectl get pods --no-headers=true | awk \'{print $1}\'") "\n"))
