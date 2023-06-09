;;; font-lock-test.el --- Tests for quakec-mode font-lock

(require 'assess)
(require 'quakec-mode)

(ert-deftest font-lock-basic-type-test ()
  (should (assess-face-at=
           "
entity	var1;
float	var2;
int	var3;
vector	var4;
string	var5;
"
           'quakec-mode '("entity" "float" "int" "vector" "string")
           'quakec-type-face)))


(ert-deftest font-lock-type-modifiers-test ()
  (should (assess-face-at=
           "
const entity var;
var entity var;
noref entity var;
local entity var;
static entity var;
nonstatic entity var;
nosave entity var;
strip entity var;
shared entity var;
optional entity var;
"
           'quakec-mode
           '("const" "var" "noref" "local" "static" "nonstatic" "nosave" "strip" "shared" "optional")
           'quakec-keyword-face)))


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


(ert-deftest font-lock-functions-qc-style-test ()
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

(ert-deftest font-lock-functions-c-style-test ()
  (should (assess-face-at=
           "
float Function1(void);

float Function2(void) = {
};

entity Function3(void) = [$param1, $param2] {
};

.vector Method4(void) = {
a + b;
};
"
           'quakec-mode '("Function1" "Function2" "Function3" "Method4")
           'quakec-function-name-face)))


(ert-deftest font-lock-function-c-style-params-test ()
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

(ert-deftest font-lock-function-qc-style-params-test ()
  (should (assess-face-at=
           "
float Function1(float param1, float param2, vector param3);

float Function2(float param4, float param5, vector param6) = {
};

entity Function3(float param7, vector param8, entity param9)  = [$arg1, $arg2] {
};

float Function2(float param10) ;

.float Method1(float param11) ;;

.float Method1(float param12, float param13) ;;


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
