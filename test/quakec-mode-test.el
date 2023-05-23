;;; quakec-mode-test.el --- Tests for quakec-mode
(require 'quakec-mode)

(ert-deftest find-definitions-test ()
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

;;; quakec-mode-test.el ends here
