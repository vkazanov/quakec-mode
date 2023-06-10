;;; which-func-test.el --- Tests for which-func

(require 'quakec-mode)

(ert-deftest which-func-test ()
  (with-quakec-temp-buffer
   "
/* a random multiline comment*/

/* a commented out function */
/* void() TestFunction1 = {
   b = insideCommentedTestFunction1
}; */

// void() TestFunction2;

/* a declaration */
void TestFunction3(void) ;

/* a definition */
float(entity targ, entity inflictor) TestFunction4 =
{
    a = insideTestFunction4
};

// betweenFunctions

/* a frame function */
void() TestFunction5 = [$pain1, $pain2]
{
    a = b;
};

"
   ;; Nothing in the beginning
   (goto-char (point-min))
   (should-not (quakec--which-func))

   ;; Should not detect stuff from within comments
   (look-at "insideCommentedTestFunction1")
   (should-not (quakec--which-func))

   (look-at "TestFunction3")
   (should-not (quakec--which-func))

   ;; this is just a declaration
   (look-at "TestFunction3")
   (should-not (quakec--which-func))

   ;; A defintion line
   (look-at "TestFunction4")
   (should (equal (quakec--which-func) "TestFunction4"))

   ;; Correctly handling the body of the function case
   (look-at "insideTestFunction4")
   (should (equal (quakec--which-func) "TestFunction4"))

   ;; Nothing when not in a funciton
   (look-at "betweenFunctions")
   (should-not (quakec--which-func))

   ;; frame function
   (search-forward "TestFunction5")
   (should (equal (quakec--which-func) "TestFunction5"))))


;;; which-func-test.el ends here
