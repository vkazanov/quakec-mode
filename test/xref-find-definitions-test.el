;;; xref-find-defintions-test.el --- Tests for quakec-mode find-definitions facility
(require 'quakec-mode)

(ert-deftest xref-find-definitions-function-test ()
  (assess-with-filesystem
      '(("test.qc" "
/* a random multiline comment*/

/* a commented out declaration*/
/* void() Declared; */

// void() Declared;

/* a declaration */
void() Declared;

/* a definition */
float(entity targ, entity inflictor) Declared =
{
    a = b
}

"))
    (assess-with-find-file "test.qc"
      (let ((defs (quakec--xref-find-definitions "Declared")))
        (should (equal (length defs) 2))
        (should (equal (xref-item-summary (nth 0 defs)) "Declared"))
        (should (equal (xref-item-summary (nth 1 defs)) "Declared"))))))

(ert-deftest xref-find-definitions-global-variable-test ()
  (assess-with-filesystem
      '(("test.qc" "
/* a commented out declaration*/
/* float Commented; */

// float AnotherCommented;

/* global definitions */
float Global0 = 1.0;
float Global1, Global2, Global3;
float Global4;

"))
    (assess-with-find-file "test.qc"
      (dolist (name '("Global0" "Global1" "Global2" "Global3" "Global4"))
        (let ((defs (quakec--xref-find-definitions name)))
          (should (equal (length defs) 1))
          (should (equal (xref-item-summary (nth 0 defs)) name)))))))

(ert-deftest xref-find-definitions-field-test ()
  (assess-with-filesystem
      '(("test.qc" "
/* a commented out declaration*/
/* .float Commented; */

// .float AnotherCommented;

/* definitions */
.float Field0;
.float Field1 = 1.0;
.float Field2 = 2.0, Field3, Field4;

"))
    (assess-with-find-file "test.qc"
      (dolist (name '("Field0" "Field1" "Field2" "Field3" "Field4"))
        (let ((defs (quakec--xref-find-definitions name)))
          (should (equal (length defs) 1))
          (should (equal (xref-item-summary (nth 0 defs)) name)))))))

(ert-deftest xref-find-definitions-all-test ()
  (assess-with-filesystem
      '(("test.qc" "
/* a random multiline comment*/

/* a commented out declaration*/
/* void() DeclaredFunction; */
/* float DeclaredGlobalVar; */
/* .float DeclaredField; */

// void() DeclaredFunction;
// float DeclaredGlobalVar;
// .float DeclaredField;

/* declarations */
void() DeclaredFunction;
float DeclaredGlobalVar;
.float DeclaredField;
.void() DeclaredMethod;

"))
    (assess-with-find-file "test.qc"
      (let ((defs-func (quakec--xref-find-definitions "DeclaredFunction"))
            (defs-global-var (quakec--xref-find-definitions "DeclaredGlobalVar"))
            (defs-local-var (quakec--xref-find-definitions "DeclaredLocalVar"))
            (defs-field (quakec--xref-find-definitions "DeclaredField")))

        (should (equal (length defs-func) 1))
        (should (equal (xref-item-summary (nth 0 defs-func)) "DeclaredFunction"))

        (should (equal (length defs-global-var) 1))
        (should (equal (xref-item-summary (nth 0 defs-global-var)) "DeclaredGlobalVar"))

        (should (equal (length defs-field) 1))
        (should (equal (xref-item-summary (nth 0 defs-field)) "DeclaredField"))))))

(ert-deftest xref-find-definitions-external-test ()
  (assess-with-filesystem
      '("progs.src"
        ("world.qc" "float worldDef;")
        ("defs.qc" "float defsDef;")
        ("test.qc" "float curDef;"))
    (assess-with-find-file "test.qc"
      ;; both external and internal definitions should be available
      (should (quakec--xref-find-definitions "worldDef"))
      (should (quakec--xref-find-definitions "defsDef"))
      (should (quakec--xref-find-definitions "curDef")))))



;;; xref-find-definitions-test.el ends here
