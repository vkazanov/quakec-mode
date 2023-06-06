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
   (let* ((index-alist (quakec--imenu-create-index))
          (function-index-alist (alist-get "*Functions*" index-alist nil nil #'string-equal))
          (method-index-alist (alist-get "*Methods*" index-alist nil nil #'string-equal)))

     (should (equal (length function-index-alist) 3))
     (should (alist-get "TestFunction3" function-index-alist nil nil #'string-equal))
     (should (alist-get "TestFunction4" function-index-alist nil nil #'string-equal))
     (should (alist-get "TestFunction5" function-index-alist nil nil #'string-equal))

     (should (equal (length method-index-alist) 1))
     (should (alist-get "TestFunction6" method-index-alist nil nil #'string-equal)))))

(ert-deftest imenu-variables-test ()
  (with-quakec-temp-buffer
   "
/* a commented out declaration*/
/* float Commented; */

// float AnotherCommented;

/* a variable */
float Global1;

/* a variable */
float Global2 = 10;

/* a field */
.float Field1;

.float Field2 = 10.1;

"
   (let* ((index-alist (quakec--imenu-create-index))
          (global-index-alist (alist-get "*Globals*" index-alist nil nil #'string-equal))
          (field-index-alist (alist-get "*Fields*" index-alist nil nil #'string-equal)))

     (should (equal (length global-index-alist) 2))
     (should (alist-get "Global1" global-index-alist nil nil #'string-equal))
     (should (alist-get "Global2" global-index-alist nil nil #'string-equal))

     (should (equal (length field-index-alist) 2))
     (should (alist-get "Field1" field-index-alist nil nil #'string-equal))
     (should (alist-get "Field2" field-index-alist nil nil #'string-equal)))))

(provide 'imenu-test)
;;; imenu-test.el ends here
