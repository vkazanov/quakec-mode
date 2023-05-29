;;; font-lock-test.el --- Tests for quakec-mode font-lock

(require 'assess)
(require 'quakec-mode)

(ert-deftest font-lock-comments-test ()
  (should (assess-face-at=
           "/* a random multiline comment continued on this line and
followed by a bunch of declarations and defintions*/

entity Variable1;

/* a commented out declaration*/
/* float() Function1; */

// void() Function2;

/* a declaration
void() Function3 = {
a = b;
}
*/
"
           'quakec-mode '("float" "void" "Function1" "Function2" "Function3")
           'font-lock-comment-face)))

(ert-deftest font-lock-builtins-test ()
  (should (assess-face-at=
           "
entity	world;
entity	self;
entity	other;
float	time;
"
           'quakec-mode '("world" "self" "other" "time")
           'quakec-builtin-face)))


(ert-deftest font-lock-functions-test ()
  (should (assess-face-at=
           "
float() Function1;

float() Function2 = {
};

entity() Function3 = [$param1, $param2] {
};

.vector() Method4 = {
a + b;
};
"
           'quakec-mode '("Function1" "Function2" "Function3" "Method4")
           'quakec-function-name-face)))


(ert-deftest font-lock-function-params-test ()
  (should (assess-face-at=
           "
float(float param1, float param2, vector param3) Function1;

float(float param4, float param5, vector param6) Function2 = {
};

entity(float param7, vector param8, entity param9) Function3 = [$arg1, $arg2] {
};

float(float param10) Function2;

.float(float param11) Method1;;

.float(float param12, float param13) Method1;;


"
           'quakec-mode '("param1" "param2" "param3"
                          "param4" "param5" "param6"
                          "param7" "param8" "param9"
                          "param10" "param11" "param12" "param13")
           'quakec-variable-name-face)))


(ert-deftest font-lock-variables-fields-test ()
  (should (assess-face-at=
           "
float Variable1;

float  Variable2 = 2;

vector Variable3;

.float Field1;

.vector Field2;

"
           'quakec-mode '("Variable1" "Variable2" "Variable3" "Field1" "Field2")
           'quakec-variable-name-face)))

;; (ert-deftest font-lock-literals-test ()
;;   (should (assess-face-at= "
;; 11
;; 2.2
;; -333
;; '444 55.5 -666'
;; \"string\"
;; "
;;                            'quakec-mode
;;                            '("11" "2.2" "-333" "444" "string")
;;                            )))


(ert-deftest font-lock-progs-test ()
  (should (assess-face-at=
           "../outputpath/progs.dat
includepath/to/include1.qc
include2.qc
include3.qc
"
           'quakec-progs-mode
           '("outputpath"
             "progs.dat"
             "includepath"
             "include1")
           '(quakec-progs-output-path-face
             quakec-progs-output-fname-face
             quakec-progs-path-face
             quakec-progs-fname-face))))

;;; font-lock-test.el ends here
