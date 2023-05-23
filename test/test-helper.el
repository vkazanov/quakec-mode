;;; test-helper.el --- Helpers for quakec-mode-test.el

(defmacro with-quakec-temp-buffer (contents &rest body)
  "Create a temporary buffer to execute a test in."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

;;; test-helper.el ends here
