;;; find-defintion-test.el --- Tests for quakec-mode find-definitions facility
(require 'quakec-mode)

(ert-deftest find-definitions-function-test ()
  (with-quakec-temp-buffer
      "
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

"
    (let ((defs (quakec--find-file-definitions "Declared")))
      (should (equal (length defs) 2))
      (should (equal (xref-item-summary (nth 0 defs)) "Declared"))
      (should (equal (xref-item-summary (nth 1 defs)) "Declared")))))


(ert-deftest find-definitions-global-variable-test ()
  (with-quakec-temp-buffer
      "
/* a commented out declaration*/
/* float Commented; */

// float AnotherCommented;

/* a global definition */
float Global;

"
    (let ((defs (quakec--find-file-definitions "Global")))
      (should (equal (length defs) 1))
      (should (equal (xref-item-summary (nth 0 defs)) "Global")))))

(ert-deftest find-definitions-field-test ()
  (with-quakec-temp-buffer
      "
/* a commented out declaration*/
/* .float Commented; */

// .float AnotherCommented;

/* a declaration */
.float Field;

"
    (let ((defs (quakec--find-file-definitions "Field")))
      (should (equal (length defs) 1))
      (should (equal (xref-item-summary (nth 0 defs)) "Field")))))

(ert-deftest find-definitions-method-test ()
  (with-quakec-temp-buffer
      "
/* a commented out declaration*/
/* .void() Commented; */

// .void() AnotherCommented;

/* a declaration */
.void() Method;

"
    (let ((defs (quakec--find-file-definitions "Method")))
      (should (equal (length defs) 1))
      (should (equal (xref-item-summary (nth 0 defs)) "Method")))))

(ert-deftest find-definitions-all-test ()
  (with-quakec-temp-buffer
      "
/* a random multiline comment*/

/* a commented out declaration*/
/* void() DeclaredFunction; */
/* float DeclaredGlobalVar; */
/* .float DeclaredField; */
/* .void() DeclaredMethod; */

// void() DeclaredFunction;
// float DeclaredGlobalVar;
// .float DeclaredField;
// .void() DeclaredMethod;

/* declarations */
void() DeclaredFunction;
float DeclaredGlobalVar;
.float DeclaredField;
.void() DeclaredMethod;

"
    (let ((defs-func (quakec--find-file-definitions "DeclaredFunction"))
          (defs-global-var (quakec--find-file-definitions "DeclaredGlobalVar"))
          (defs-local-var (quakec--find-file-definitions "DeclaredLocalVar"))
          (defs-field (quakec--find-file-definitions "DeclaredField"))
          (defs-method (quakec--find-file-definitions "DeclaredMethod")))

      (should (equal (length defs-func) 1))
      (should (equal (xref-item-summary (nth 0 defs-func)) "DeclaredFunction"))

      (should (equal (length defs-global-var) 1))
      (should (equal (xref-item-summary (nth 0 defs-global-var)) "DeclaredGlobalVar"))

      (should (equal (length defs-field) 1))
      (should (equal (xref-item-summary (nth 0 defs-field)) "DeclaredField"))

      (should (equal (length defs-method) 1))
      (should (equal (xref-item-summary (nth 0 defs-method)) "DeclaredMethod")))))



;;; find-definitions-test.el ends here
