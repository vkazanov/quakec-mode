;;; project-root-test.el --- Tests for quakec-mode project root finding functions

(require 'assess)

(ert-deftest relative-path-x-test ()
  (assess-with-filesystem
      '("proj/progs.src"
        ;; project files
        ("proj/dir1/f1.qc" "float def1;")
        ("proj/dir1/dir2/f2.qc" "float def2;")
        ("proj/f3.qc" "vector def3;")
        ;; outside of the project
        ("f4" "entity world;"))
    (assess-with-find-file "proj/dir1/f1.qc"
      (should (equal (quakec--relative-path)
                     "dir1/f1.qc")))
    (assess-with-find-file "proj/dir1/dir2/f2.qc"
      (should (equal (quakec--relative-path)
                     "dir1/dir2/f2.qc")))

    (assess-with-find-file "proj/f3.qc"
      (should (equal (quakec--relative-path)
                     "f3.qc")))

    (assess-with-find-file "f4.qc"
      (should-error (quakec--relative-path)))))

(ert-deftest find-project-root-test ()
  (assess-with-filesystem
      '("proj/progs.src"

        ;; project files
        ("proj/dir1/f1.qc" "float def1;")
        ("proj/dir1/dir2/f2.qc" "float def2;")
        ;; outside of the project
        ("f4" "entity world;"))
    (let ((projroot (file-name-as-directory (expand-file-name "proj" default-directory))))
      (assess-with-find-file "proj/dir1/f1.qc"
        (should (equal (quakec--find-project-root)
                       projroot)))

      (assess-with-find-file "proj/dir1/dir2/f2.qc"
        (should (equal (quakec--find-project-root)
                       projroot)))

      (assess-with-find-file "f4"
        (should-error (quakec--find-project-root))))))

(ert-deftest project-file-exists-test ()
  (assess-with-filesystem
      '("proj/progs.src"

        ;; project files
        "proj/f0.qc"
        "proj/dir1/f1.qc"
        "proj/dir1/dir2/f2.qc"

        ;; outside of the project
        "outside.qc")

    (assess-with-find-file "proj/f0.qc"
      (should (quakec--project-file-exists "f0.qc"))
      (should (quakec--project-file-exists "dir1/f1.qc"))
      (should (quakec--project-file-exists "dir1/dir2/f2.qc"))
      (should-not (quakec--project-file-exists "outside.qc")))

    (assess-with-find-file "outside.qc"
      (should-error (quakec--project-file-exists "outside.qc")))))


;;; project-root-test.el ends here
