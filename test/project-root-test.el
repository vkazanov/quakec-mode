;;; project-root-test.el --- Tests for quakec-mode project root finding functions

(ert-deftest quakec-find-project-root-test ()
  (with-temp-buffer
    (let (temp-dir)
      (unwind-protect
          (progn
            (setq temp-dir (make-temp-file "quakec-test" t))

            (setq default-directory temp-dir)
            (should-error (quakec--find-project-root))

            (write-region "" nil (expand-file-name "progs.src" temp-dir ))
            (should (equal (quakec--find-project-root)
                           (file-name-as-directory temp-dir))))
        (delete-directory temp-dir t)))))

(ert-deftest quakec-relative-path-test ()
  (let* ((fake-project-root (make-temp-file "quakec-project-" t))
         (fake-src-file (expand-file-name quakec-project-source fake-project-root))
         (fake-src-subdir (expand-file-name "subdir" fake-project-root))
         (fake-src-file-subdir (expand-file-name "subfile.qc" fake-src-subdir)))
    (unwind-protect
        (progn
          (write-region "" nil fake-src-file)
          (make-directory fake-src-subdir)

          (write-region "" nil fake-src-file-subdir)
          (with-current-buffer (find-file-noselect fake-src-file)
            (should (equal (quakec--relative-path (buffer-file-name)) quakec-project-source)))

          (with-current-buffer (find-file-noselect fake-src-file-subdir)
            (should (equal (quakec--relative-path (buffer-file-name)) (concat "subdir/" "subfile.qc")))))
      (delete-directory fake-project-root t))))

;;; project-root-test.el ends here
