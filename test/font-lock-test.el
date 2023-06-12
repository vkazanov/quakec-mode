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
int var6;
"
           'quakec-mode '("entity" "float" "int" "vector" "string")
           'quakec-type-face)))


(ert-deftest font-lock-type-modifiers-test ()
  (should (assess-face-at=
           "
const entity v;
var entity v;
noref entity v;
local entity v;
static entity v;
nonstatic entity v;
nosave entity v;
strip entity v;
shared entity v;
optional entity v;
"
           'quakec-mode
           '("const" "var" "noref" "local" "static" "nonstatic" "nosave" "strip" "shared" "optional")
           'quakec-keyword-face)))


(ert-deftest font-lock-basic-type-modifiers-test ()
  (should (assess-face-at=
           "
entity	var1;
float	var2;
const entity	var3;
static float	var4;
"
           'quakec-mode '("var1"
                          "var2"
                          "var3"
                          "var4")
           '(quakec-variable-name-face
             quakec-variable-name-face
             quakec-variable-name-face
             quakec-variable-name-face))))

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
           'quakec-comment-face)))

(ert-deftest font-lock-builtins-test ()
  "Builtins should be highlighted with a `quakec-builtin-face' but
only in non-defining context."
  (should (assess-face-at=
           "
float (void) fname = {
    world + self + other + time;
}
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

"
           'quakec-mode '("Function1" "Function2" "Function3")
           'quakec-function-name-face)))

(ert-deftest font-lock-functions-c-style-test ()
  (should (assess-face-at=
           "
float Function1(void);

float Function2(void) = {
};

entity Function3(void) = [$param1, $param2] {
};
"
           'quakec-mode '("Function1" "Function2" "Function3")
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

"
           'quakec-mode '("param1" "param2" "param3"
                          "param4" "param5" "param6"
                          "param7" "param8" "param9"
                          "param10")
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

"
           'quakec-mode '("param1" "param2" "param3"
                          "param4" "param5" "param6"
                          "param7" "param8" "param9"
                          "param10")
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


(ert-deftest font-lock-variables-fields-comments-test ()
  "Check comments following definitions."
  (should (assess-face-at=
           "
/*
float comment;
*/

float Variable1; // varcomment

float  Variable2 = 2;

vector Variable3;

.float Field1; // fieldcomment

.vector Field2;

"
           'quakec-mode '("comment" "varcomment" "fieldcomment" )
           'quakec-comment-face)))


(ert-deftest font-lock-multiple-global-variables-test ()
  "Check global variable/constant definition highlighting. "
  (should (assess-face-at=
           "
float var_var1 = 1, var2 = 2.0;
const float var3, var4, var5;
"
           'quakec-mode '("var_var1" "var2" "var3" "var4" "var5")
           'quakec-variable-name-face)))


(ert-deftest font-lock-multiple-local-variables-test ()
  "Check function-local variable/constant definition highlighting.
Note how it checks both whitespace and tab indentation. "
  (should (assess-face-at=
           "
float fname(float p1, floatp2) = {
    float var_var1 = 1, var2 = 2.0;
	const float var3, var4, var5;
};
"
           'quakec-mode '("var_var1" "var2" "var3" "var4" "var5")
           'quakec-variable-name-face)))

(ert-deftest font-lock-multiple-fields-test ()
  "Check multple field definitions per line. Same as for globals
but with a dot prefix in type."
  (should (assess-face-at=
           "
.float var_var1 = 1, var2 = 2.0;
const .float var3, var4, var5;
};
"
           'quakec-mode '("var_var1" "var2" "var3" "var4" "var5")
           'quakec-variable-name-face)))

;; TODO: c-mode doesn't help here
;;
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


(ert-deftest font-lock-pragma-test ()
  (should (assess-face-at=
           "
#define TRUE FALSE
#if TRUE
#elif
#else

"
           'quakec-mode '("define" "#if" "#elif" "#else")
           'quakec-preprocessor-face)))

(ert-deftest font-lock-progs-test ()
  (should (assess-face-at=
           "../outputpath/progs.dat
#define TEST
include_path/to/include1.qc
include2.qc
include_file3.qc
path/to/include_file4.qc
"
           'quakec-progs-mode
           '("outputpath"
             "progs.dat"

             "#define"

             "include_path"
             "include1.qc"
             "include2.qc"
             "include_file3"
             "include_file4.qc")
           '(;; output line
             quakec-progs-output-path-face
             quakec-progs-output-fname-face

             ;; preprocessor
             quakec-preprocessor-face

             ;; includes
             quakec-progs-path-face
             quakec-progs-fname-face
             quakec-progs-fname-face
             quakec-progs-fname-face
             quakec-progs-fname-face))))

;;; font-lock-test.el ends here
