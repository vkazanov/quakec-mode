;;; eldoc-test.el --- Tests for eldoc


(ert-deftest eldoc-test ()
  (with-quakec-temp-buffer
   "
/* a random multiline comment*/

/* a commented out function */
/*
void() TestFunction0 = {
   b = insideCommentedTestFunction1
};
*/

/* void() TestFunction1 = {
   b = insideCommentedTestFunction1
}; */

// void() TestFunction2;

/* a declaration */
void() TestFunction3;

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

/* a method */
.void() TestFunction6 =
{
    a = unknownSymbol
};

"
   ;; Nothing in the beginning
   (goto-char (point-min))
   (should-not (quakec--eldoc-function))

   ;; Should not detect stuff from within comments
   (look-at "TestFunction0")
   (should-not (quakec--eldoc-function))

   (look-at "TestFunction1")
   (should-not (quakec--eldoc-function))

   (look-at "TestFunction2")
   (should-not (quakec--eldoc-function))

   ;; a declaration line
   (look-at "Function3")
   (should (equal (quakec--eldoc-function) "void() TestFunction3"))

   ;; A defintion line
   (look-at "Function4")
   (should (equal (quakec--eldoc-function) "float(entity targ, entity inflictor) TestFunction4"))

   ;; Nothing when not in a funciton, and not a known symbol
   (look-at "betweenFunctions")
   (should-not (quakec--eldoc-function))

   ;; frame function
   (look-at "TestFunction5")
   (should (equal (quakec--eldoc-function) "void() TestFunction5"))

   ;; Method function
   (look-at "TestFunction6")
   (should (equal (quakec--eldoc-function) ".void() TestFunction6"))

   ;; unknown symbol
   (look-at "unknownSymbol")
   (should-not (quakec--eldoc-function))))


;;; eldoc-test.el ends here
