;;; imenu-test.el --- test imenu facilities          -*- lexical-binding: t; -*-


(ert-deftest imenu-functions-test ()
  (with-quakec-temp-buffer
   "
/* a random multiline comment*/

/* a commented out function */
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
    a = b
};

"
   (let ((index-alist (quakec--imenu-create-index)))
     (should (equal (length index-alist) 4))
     (should (alist-get "TestFunction3" index-alist nil nil #'string-equal))
     (should (alist-get "TestFunction4" index-alist nil nil #'string-equal))
     (should (alist-get "TestFunction5" index-alist nil nil #'string-equal))
     (should (alist-get "TestFunction6" index-alist nil nil #'string-equal)))))

(ert-deftest imenu-variables-test ()
  (with-quakec-temp-buffer
   "
/* a commented out declaration*/
/* float Commented; */

// float AnotherCommented;

/* a variable */
float Global;

/* a field */
.float Field;

"
   (let ((index-alist (quakec--imenu-create-index)))
     (should (equal (length index-alist) 2))
     (should (alist-get "Global" index-alist nil nil #'string-equal))
     (should (alist-get "Field" index-alist nil nil #'string-equal)))))

(provide 'imenu-test)
;;; imenu-test.el ends here
