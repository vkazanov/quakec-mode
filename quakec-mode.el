;;; quakec-mode.el --- Major mode for QuakeC  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2023 Vladimir Kazanov

;; Author: Vladimir Kazanov
;; Version: 0.1
;; Keywords: games, languages
;; URL: https://github.com/vkazanov/quakec-mode
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing QuakeC and QuakeC src files.
;;
;; The dialect is QuakeC as described in the QuakeC Reference Manual.
;; In addition to the Vanilla dialect some popular FTEQCC and GMQCC
;; extentions are supported, e.g. C-style function definitions. The
;; mode provides imenu, which-func and Eldoc support, as well an xref
;; backend for finding definition and a completion-at-point function
;; for local symbol completion function. By default FTEQCC is used as
;; a compile command.
;;
;; Apart from *.qc files a support for original *.src format is
;; provided through the bundled `quakec-progs-mode'.

;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'generic-x)

(defconst quakec-fteqcc-compile-command "fteqcc -Wall "
  "An FTEQCC compile command for QuakeC.")

(defconst quakec-gmqcc-compile-command "gmqcc -Wall -nocolor "
  "An GMQCC compile command for QuakeC.")

;;
;;; Customization
;;

(defgroup quakec nil
  "Support for QuakeC code."
  :link '(url-link "https://github.com/vkazanov/quakec-mode")
  :group 'languages)

(defcustom quakec-project-source "progs.src"
  "QuakeC project root file to use for location of a root directory."
  :type 'string
  :group 'quakec)

(defcustom quakec-project-definition-files '("defs.qc" "world.qc")
  "File paths relative to project root used in definition lookups."
  :type '(repeat string)
  :group 'quakec)

(defcustom quakec-compile-command quakec-fteqcc-compile-command
  "A default command to use for compiling QuakeC project."
  :type 'string
  :group 'quakec)

(defcustom quakec-flymake-fteqcc-cmd '("fteqcc" "-Wall")
  "Strings to use as a command in the FTEQCC flymake backend."
  :type '(repeat string)
  :group 'quakec)

(defcustom quakec-flymake-gmqcc-cmd '("gmqcc" "-Wall" "-nocolor" "-std=fteqcc")
  "Strings to use as a command when in the QMQCC flymake backend."
  :type '(repeat string)
  :group 'quakec)

;;
;;; Faces
;;

(defvar quakec-comment-face 'quakec-comment-face)
(defface quakec-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments."
  :group 'quakec)

(defvar quakec-keyword-face 'quakec-keyword-face)
(defface quakec-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords."
  :group 'quakec)

(defvar quakec-type-face 'quakec-type-face)
(defface quakec-type-face
  '((t :inherit font-lock-type-face))
  "Face for types."
  :group 'quakec)

(defvar quakec-constant-face 'quakec-constant-face)
(defface quakec-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for constants."
  :group 'quakec)

(defvar quakec-builtin-face 'quakec-builtin-face)
(defface quakec-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for builtins."
  :group 'quakec)

(defvar quakec-preprocessor-face 'quakec-preprocessor-face)
(defface quakec-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor pragmas."
  :group 'quakec)

(defvar quakec-variable-name-face 'quakec-variable-name-face)
(defface quakec-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variable names."
  :group 'quakec)

(defvar quakec-function-name-face 'quakec-function-name-face)
(defface quakec-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names."
  :group 'quakec)

;;; progs.src faces

(defvar quakec-progs-output-path-face 'quakec-progs-output-path-face)
(defface quakec-progs-output-path-face
  '((t :inherit font-lock-string-face))
  "Face for paths in QuakeC progs.src files."
  :group 'quakec)

(defvar quakec-progs-output-fname-face 'quakec-progs-output-fname-face)
(defface quakec-progs-output-fname-face
  '((t :inherit font-lock-keyword-face))
  "Face for output filename in QuakeC progs.src files."
  :group 'quakec)

(defvar quakec-progs-path-face 'quakec-progs-path-face)
(defface quakec-progs-path-face
  '((t :inherit font-lock-string-face))
  "Face for paths in QuakeC progs.src files."
  :group 'quakec)

(defvar quakec-progs-fname-face 'quakec-progs-fname-face)
(defface quakec-progs-fname-face
  '((t :inherit font-lock-string-face))
  "Face for filenames in QuakeC progs.src files."
  :group 'quakec)

;;
;;; Syntax highlighting and (limited) parsing regexps
;;

(defvar quakec--pragmas-re
  (rx-to-string '(seq line-start (or "#define" "#append" "#undef" "#if" "#ifdef" "#ifndef" "#elif" "#else" "#endif" "#pragma")))
  "A regexp matching pragmas.")

(defvar quakec--frame-pragmas-re
  (regexp-opt
   '("$frame" "$framegroupstart" "$framegroupend" "$flags" "$base" "$cd" "$modelname" "$origin" "$scale" "$skin")
   'symbols)
  "A regexp matching legacy frame pragmas.")

(defvar quakec--builtins-re
  (regexp-opt
   '(
     ;;  Major globals
     "world" "self" "other" "time"

     ;; Basic math
     "anglemod" "ceil" "fabs" "floor" "ftos" "rint" "random"

     ;; Vector math
     "makevectors" "normalize" "vlen" "vectoangles" "vectoyaw" "vtos"

     ;; Collision
     "checkbottom" "checkpos" "pointcontents" "traceline"

     ;; Combat
     "aim" "checkclient" "particle"

     ;; Console
     "cvar_set" "dprint" "localcmd" "cvar"

     ;; Debug
     "debug" "coredump" "eprint" "error" "objerror" "traceoff" "traceon"

     ;; Entity management
     "find" "findradius" "lightstyle" "makestatic" "nextent" "remove" "setmodel" "spawn"

     ;; Movement
     "ChangeYaw" "droptofloor" "movetogoal" "setorigin" "setsize" "walkmove"

     ;; Message
     "bprint" "centerprint" "sprint"

     ;; Network
     "WriteAngle" "WriteByte" "WriteChar" "WriteCoord" "WriteEntity" "WriteShort" "WriteString" "WriteLong" "multicast"

     "ReadAngle" "ReadByte" "ReadChar" "ReadCoord" "ReadShort" "ReadString" "ReadLong" "ReadFloat"


     ;; Precaching
     "precache_file" "precache_model" "precache_sound"

     ;; Server-related
     "changelevel" "setspawnparms" "stuffcmd"

     ;; Sound
     "ambientsound" "sound"

     ;; Mandatory functions called by Quake itself
     "PlayerPostThink" "PlayerPreThink" "ClientConnect" "ClientDisconnect" "ClientKill" "PutClientInServer" "SetChangeParms" "SetNewParms"
     "main" "StartFrame"

     ;; entity functions and relevant fields
     "nextthink" "think" "touch" "use" "blocked")
   'symbols)
  "A regexp matching builtin functions.")

(defvar quakec--constants-re
  (regexp-opt '("TRUE" "FALSE") 'symbols)
  "A regexp matching constants.")

(defvar quakec--keywords-re
  (regexp-opt '("if" "else" "for" "do" "while" "switch" "case" "default" "break" "continue" "return") 'symbols)
  "A regexp matching keywords.")

(defvar quakec--type-modifier-keywords-re
  (regexp-opt '("const" "var" "noref" "local" "static" "nonstatic" "nosave" "strip" "shared" "optional") 'symbols)
  "A regexp matching type modifier keywords.")

(defvar quakec--basic-type-re
  (regexp-opt '("void" "entity" "float" "vector" "string" "int") 'symbols)
  "A regexp catching basic types.")

(defvar quakec--name-re
  "[[:alpha:]][[:alnum:]_]+\\b"
  "A regexp catching symbol names.")


(defvar quakec--function-parameter-left-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
		      ;; return type
		      (regexp ,quakec--basic-type-re)
		      (zero-or-more whitespace)

                      ;; maybe name (in C-style)
                      (opt (regexp ,quakec--name-re)
                           (zero-or-more whitespace))

                      ;; parameter list start
                      "("))
  "A regexp catching the prefix of function declaration.")

(defvar quakec--function-parameter-re
  (rx-to-string `(seq (zero-or-more whitespace)
                      ;; optional delimieter
                      (zero-or-more whitespace)
                      (opt ",")
                      (zero-or-more whitespace)

		      ;; type
		      (regexp ,quakec--basic-type-re)
		      (zero-or-more whitespace)

                      ;; parameter name
                      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching a single function formal parameter.")

(defvar quakec--function-qc-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
		      ;; return type
		      (regexp ,quakec--basic-type-re)
		      (zero-or-more whitespace)

		      ;; parameter list
		      "(" (zero-or-more (regexp ".")) ")"
		      (zero-or-more whitespace)

		      ;; function name
		      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching a QuakeC-style function name.")

(defvar quakec--function-c-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
		      ;; return type
		      (regexp ,quakec--basic-type-re)
		      (zero-or-more whitespace)

                      ;; function name
		      (group-n 1 (regexp ,quakec--name-re))
                      (zero-or-more whitespace)

		      ;; parameter list
		      "(" (zero-or-more (regexp ".")) ")"))
  "A regexp catching a C-style function name.")

(defvar quakec--function-frame-params-re
  (rx-to-string `(seq
                  ;; a usual function as a prefix
                  (regexp ,quakec--function-qc-re)

                  ;; followed by 2 additional parameters
                  (zero-or-more whitespace) "="

                  (zero-or-more whitespace) "["

                  (zero-or-more whitespace)
                  (group-n 2 (seq "$" (regexp ,quakec--name-re)))

                  (zero-or-more whitespace) ","

                  (zero-or-more whitespace)
                  (group-n 3 (regexp ,quakec--name-re))

                  (zero-or-more whitespace) "]"))
  "A regexp catching a frame function additional params.")

(defvar quakec--global-variable-re
  (rx-to-string `(seq line-start
                      ;; optional type modifier
                      (opt (regexp ,quakec--type-modifier-keywords-re)
                           (zero-or-more whitespace))

                      ;; global variable type
                      (regexp ,quakec--basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec--name-re))
                      (zero-or-more whitespace)

                      ;; non a c-style function
                      (regexp "[^(]")))
  "A regexp catching a global variable definition.")

(defvar quakec--local-variable-re
  (rx-to-string `(seq line-start (one-or-more whitespace)

                      ;; optional type modifier
                      (opt (regexp ,quakec--type-modifier-keywords-re)
                           (zero-or-more whitespace))

                      ;; global variable type
                      (regexp ,quakec--basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec--name-re))
                      (zero-or-more whitespace)))
  "A regexp catching a local variable definition.

This is essentially the same as `quakec--global-variable-re' but
with leading whitespace and without the need to rule out function
definitions.

NOTE: Very fragile and unreliable, even a partial parser would do
a much better job here.")

(defvar quakec--variable-name-re
  (rx-to-string `(seq (zero-or-more whitespace)
                      ;; optional delimieter
                      (opt ",")
                      (zero-or-more whitespace)

                      ;; parameter name
                      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching variable names in a list of names defined.")

(defvar quakec--field-re
  (rx-to-string `(seq line-start
                      ;; optional type modifier
                      (opt (regexp ,quakec--type-modifier-keywords-re)
                           (zero-or-more whitespace))

                      ;; global variable type
                      "."
                      (regexp ,quakec--basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec--name-re))
                      (zero-or-more whitespace)

                      ;; non a c-style function
                      (regexp "[^(]")))
  "A regexp catching an entity field declaration.
Same as `quakec--global-variable-re' but with a type prefixed
with a dot (\".\")")

;;
;;; Syntax highlighting (font-lock)
;;

(defvar quakec--font-lock-keywords
  `((,quakec--pragmas-re . 'quakec-preprocessor-face)
    (,quakec--frame-pragmas-re . 'quakec-preprocessor-face)

    (,quakec--keywords-re . 'quakec-keyword-face)
    (,quakec--basic-type-re . 'quakec-type-face)
    (,quakec--type-modifier-keywords-re . 'quakec-keyword-face)
    (,quakec--constants-re . 'quakec-constant-face)
    (,quakec--builtins-re . 'quakec-builtin-face)
    ;; TODO: both global and local fail looking for multiple line
    ;; variable definition because of the semicolon in the end of the regexp

    ;; find a variable definition then go back to the beginning of the
    ;; variable name and then look for comma-separated var names
    (,quakec--global-variable-re
     (,quakec--variable-name-re (goto-char (match-beginning 1)) nil
                                (1 'quakec-variable-name-face)))
    ;; locals work the same way as globals
    (,quakec--local-variable-re
     (,quakec--variable-name-re (goto-char (match-beginning 1)) nil
                                (1 'quakec-variable-name-face)))
    ;; same for fields
    (,quakec--field-re
     (,quakec--variable-name-re (goto-char (match-beginning 1)) nil
                                (1 'quakec-variable-name-face)))
    (,quakec--function-c-re . (1 'quakec-function-name-face ))
    (,quakec--function-qc-re . (1 'quakec-function-name-face ))
    (,quakec--function-parameter-left-re (,quakec--function-parameter-re nil nil (1 'quakec-variable-name-face)))
    (,quakec--function-frame-params-re (2 'quakec-variable-name-face)
                                       (3 'quakec-variable-name-face))))

;;
;;; Cached definition lookup
;;

(cl-defstruct (quakec--definition (:constructor nil)
                                  (:constructor quakec--definition-create (name file beg end line col signature deftype))
                                  (:copier nil))
  "A definition location and signature meant to be used in various
quakec-mode facilities relying on defition search."
  name
  file
  beg end
  line col
  signature
  deftype)

(defvar-local quakec--buffer-definitions-cache nil
  "A cache of QuakeC definitions.
The cache maps ids to definition locations described by
`quakec-definition'.")

(defun quakec--update-definitions ()
  "Build a cache of definitions for the current buffer."
  (let ((externaldefs quakec-project-definition-files)
        newcache)

    ;; Check for definitions in important external files
    ;;

    ;; do not look for project files when not in a project
    (when (quakec--project-p)
      (dolist (projfpath externaldefs)
        ;; (message "fpath: %s" projfpath)

        ;; Only inject external defintions when its not the current
        ;; buffer that's injected and the file exists. Also skip
        ;; recursing if visiting special external files
        ;;
        (when (and (quakec--project-file-exists projfpath)
                   (not (member (quakec--relative-path) externaldefs)))
          (save-excursion
            ;; check if the file is already visited by a buf
            (let* ((abspath (quakec--project-file-abs-path projfpath))
                   (keep (find-buffer-visiting abspath)))

              ;; This should extract definitions for the external file
              (find-file abspath)

              ;; TODO: this runs definition retrieval a second time. FIX!
              (setq newcache (or newcache (make-hash-table :test 'equal)))
              (quakec--update-buffer-definitions newcache)

              ;; only kill the file buffer if the file wasn't openned
              ;; yet
              (unless keep (kill-buffer)))))))

    ;; Now check the current buffer definitions
    ;;

    (setq newcache (or newcache (make-hash-table :test 'equal)))
    (quakec--update-buffer-definitions newcache)
    (setq quakec--buffer-definitions-cache newcache)))

(defun quakec--update-buffer-definitions (cache-ht)
  "Fill the QuakeC definition cache CACHE-HT (a hash table)."
  (cl-assert (hash-table-p cache-ht))
  (save-excursion
    ;; functions are easy: just look up regexps
    ;;

    ;; QC-style functions
    ;;
    (goto-char (point-min))
    (while (re-search-forward quakec--function-qc-re nil t)
      (unless (nth 4 (syntax-ppss))
        (let* ((name (match-string-no-properties 1))
               (file (buffer-file-name))
               (beg (match-beginning 0))
               (end (match-end 0))
               (line (line-number-at-pos))
               ;; TODO: just save 0 for now
               (col 0)
               (signature (match-string 0))
               (newdef (quakec--definition-create
                        name file
                        beg end
                        line col
                        signature 'function)))
          (push newdef (gethash name cache-ht)))))

    ;; C-style functions
    ;;
    (goto-char (point-min))
    (while (re-search-forward quakec--function-c-re nil t)
      (unless (nth 4 (syntax-ppss))
        (let* ((name (match-string-no-properties 1))
               (file (buffer-file-name))
               (beg (match-beginning 0))
               (end (match-end 0))
               (line (line-number-at-pos))
               ;; TODO: just save 0 for now
               (col 0)
               (signature (match-string 0))
               (newdef (quakec--definition-create
                        name file
                        beg end
                        line col
                        signature 'function)))
          (push newdef (gethash name cache-ht)))))

    ;; When looking up global variables and fields we can have
    ;; multiple symbol definitions per line, i.e. float a,b,c;. So
    ;; find an anchor first, match next.

    ;; Variables
    ;;
    (goto-char (point-min))
    (while (re-search-forward quakec--global-variable-re nil t)
      (unless (nth 4 (syntax-ppss))
        ;; Remember everything but the name as it's the only thing
        ;; that differs between single line definitions.
        (let* ((file (buffer-file-name))
               (beg (match-beginning 0))
               (end (match-end 0))
               (line (line-number-at-pos))
               ;; TODO: just save 0 for now
               (col 0)
               (signature (match-string 0))
               (lineend (line-end-position)))
          ;; Backtrack to the first variable name on the line (we know
          ;; it's there because of the anchor match above. Limit
          ;; search by LINEEND. NOTE: This missed a few cases (float
          ;; a, \n b, \n c) but should work most of the time.
          (goto-char (match-beginning 1))
          (while (re-search-forward quakec--variable-name-re lineend t)
            (let* ((name (match-string-no-properties 1))
                   (newdef (quakec--definition-create
                            name
                            file
                            beg end
                            line col
                            signature 'global)))
              (push newdef (gethash name cache-ht)))))))

    ;; Fields (same as variables)
    ;;
    (goto-char (point-min))
    (while (re-search-forward quakec--field-re nil t)
      (unless (nth 4 (syntax-ppss))
        (let* ((file (buffer-file-name))
               (beg (match-beginning 0))
               (end (match-end 0))
               (line (line-number-at-pos))
               ;; TODO: just save 0 for now
               (col 0)
               (signature (match-string 0))
               (lineend (line-end-position)))
          (goto-char (match-beginning 1))
          (while (re-search-forward quakec--variable-name-re lineend t)
            (let* ((name (match-string-no-properties 1))
                   (newdef (quakec--definition-create
                            name
                            file
                            beg end
                            line col
                            signature 'field)))
              (push newdef (gethash name cache-ht)))))))))

(defun quakec--get-definition-positions (definition-type)
  "Retrieve a list of all known buffer definitions.

Returns a list cons cells mapping definitions of type
DEFINITION-TYPE to positions in cons cells."
  (let (defpositions)
    (cl-loop for deflist being the hash-values of quakec--buffer-definitions-cache
             do
             (dolist (def deflist)
               (when (and (eq (quakec--definition-deftype def) definition-type)
                          (equal (quakec--definition-file def) (buffer-file-name)))
                 (push (cons (quakec--definition-name def) (quakec--definition-beg def))
                       defpositions))))
    defpositions))

(defun quakec--get-buffer-definition-names ()
  "Retrieve all known buffer definition names."
  (hash-table-keys quakec--buffer-definitions-cache))

(defun quakec--find-buffer-definitions (name)
  "Retrieve a NAME definition list from definition cache by NAME."
  (gethash name quakec--buffer-definitions-cache))

;;
;;; Completion-at-point backend
;;

(defun quakec-completion-at-point ()
  "Provide completion for local QuakeC symbols."
  (let (complete-function symbol-bounds symbols-start symbols-end)
    (setq symbol-bounds (bounds-of-thing-at-point 'symbol))
    (when (and (setq symbols-start (car symbol-bounds))
               (setq symbols-end (cdr symbol-bounds)))
      (setq complete-function (lambda (_) (quakec--get-buffer-definition-names)))
      (list symbols-start symbols-end
            (completion-table-dynamic complete-function)
            :exclusive 'no))))

;;
;;; Xref-based definition lookup
;;

(defun quakec-xref-backend ()
  "Buffer-local QuakeC Xref backend."
  'quakec)

(defun quakec--xref-find-definitions (identifier)
  "Find definitions of an IDENTIFIER in the current buffer."
  (let (matches)
    (dolist (def (quakec--find-buffer-definitions identifier))
      (when-let ((pos (quakec--definition-beg def))
                 (file (quakec--definition-file def))
                 (line (quakec--definition-line def))
                 (col (quakec--definition-col def))
                 (fileloc (xref-make
                           identifier
                           (xref-make-file-location file line col))))
        (push fileloc matches)))
    matches))

(cl-defmethod xref-backend-definitions ((_ (eql quakec)) identifier)
  "QuakeC file-level definition finding Xref BACKEND.
IDENTIFIER is a symbol to lookup."
  (quakec--xref-find-definitions identifier))

;;
;;; Imenu
;;

(defun quakec--imenu-create-index ()
  "Build an Imenu index of file definitions.
Return an index alist as required by
`imenu-create-index-function'."
  (let ((index-alist))
    (cl-loop
     for (deftyp deftypname)
     in '((function "*Functions*")
          (global "*Globals*")
          (field "*Fields*"))
     do
     (when-let ((defs (quakec--get-definition-positions deftyp)))
       (push (cons deftypname defs) index-alist)))
    index-alist))

;;
;;; Eldoc
;;

(defun quakec--eldoc-function ()
  "Show a definition string for the symbol at point."
  (when-let*
      ((symbol-name (thing-at-point 'symbol 'no-props))
       (deflist (quakec--find-buffer-definitions symbol-name))
       ;; just pick the first definition in the list of definitions
       (def (car deflist)))
    (quakec--definition-signature def)))

(defun quakec--after-save-hook ()
  "Update QuakeC definitions cache after saving the file."
  (when (eq major-mode 'quakec-mode)
    (quakec--update-definitions)))

;;
;;; Which function mode support
;;

(defun quakec--which-func ()
  "Find the current function name in a QuakeC buffer."
  (save-excursion
    (let ((original-position (point)))
      ;; Move to the beginning of the current defun
      (beginning-of-defun)
      ;; Check if we are within a function body
      (if (and (or (looking-at quakec--function-qc-re)
                   (looking-at quakec--function-c-re))
               (save-match-data
                 (end-of-defun)
                 (<= original-position (point))))
          (match-string-no-properties 1)
        nil))))

;;
;;; Project utils
;;

(defun quakec--find-project-root ()
  "Find a path to the root of the QuakeC project.
The root is indicated by the file specified in
`quakec-project-source' (`progs.src') file. Signals a
`user-error' if not in a project."
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               quakec-project-source)))
    (unless root
      (user-error "Not in a QuakeC project"))
    root))

(defun quakec--project-p ()
  "Check if working within a QuakeC project.
The root is indicated by the file specified in
`quakec-project-source' (`progs.src') file. Signals a
`user-error' if not in a project."
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               quakec-project-source)))
    root))

(defun quakec--project-file-abs-path (fpath)
  "Return an absolute path for a project-relative FPATH."
  (expand-file-name fpath (quakec--find-project-root)))

(defun quakec--project-file-exists (fpath)
  "Return t if a file exists in the current project.
FPATH - relative project path check."
  (file-exists-p (quakec--project-file-abs-path fpath)))

(defun quakec--relative-path (&optional fpath)
  "Create an relative path for a project file.
FPATH - file path relative to project root."
  (file-relative-name (or fpath (buffer-file-name))
                      (quakec--find-project-root)))

;;
;;; On-the-fly syntax checking
;;


(defvar-local quakec--flymake-proc nil)

(defun quakec--flymake-fteqcc-build-diagnostic-re (fpath)
  "Build a regular expression catching FTEQCC diagnostic messages.
FPATH - a path to a file to extract diagnostic messages for."
  (format "^\\(?:%s\\|-\\):\\([0-9]+\\): \\(.*\\)$" (regexp-quote fpath)))

(defun quakec--flymake-gmqcc-build-diagnostic-re (fpath)
  "Build a regular expression catching GMQCC diagnostic messages.
FPATH - a path to a file to extract diagnostic messages for."
  (format "^\\(?:%s\\|-\\):\\([0-9]+\\):[0-9]+: \\(.*\\)$" (regexp-quote fpath)))

(defun quakec--flymake-collect-diagnostics (report-fn sourcebuf diag-re)
  "Collect Flymake diagnostics for a given buffer.

REPORT-FN - a function that would report the error.
SOURCEBUF - a buffer to find errors in.
DIAG-RE - a regexp matching diagnostic messages."
  (goto-char (point-min))
  (cl-loop
   while (search-forward-regexp diag-re nil t)
   for msg = (match-string 2)
   for (beg . end) = (flymake-diag-region sourcebuf (string-to-number (match-string 1)))
   for type = (if (string-match "^warning" msg)
                  :warning
                :error)
   collect (flymake-make-diagnostic sourcebuf
                                    beg end
                                    type
                                    msg)
   into diags
   finally (funcall report-fn diags)))

(defun quakec--make-flymake-backend (compiler-exec command diag-re-builder)
  "Create a new flymake backend for a specific QuakeC compiler.
COMPILER-EXEC - a compiler executable to use.
COMMAND - command to run the compiler.
DIAG-RE-BUILDER - a function to call to generate the diagnostic
regular expression."
  (lambda (report-fn &rest _args)
    ;; Make sure a compiler is available
    (unless (executable-find
             compiler-exec) (error "Cannot find %s" compiler-exec))

    ;; Reset the cached process
    (when (process-live-p quakec--flymake-proc)
      (kill-process quakec--flymake-proc))

    ;; Launch the process
    (let* ((sourcebuf (current-buffer))
           (source-path (quakec--relative-path))
           (diag-re (funcall diag-re-builder source-path))
           (default-directory (quakec--find-project-root)))
      (save-restriction
        (widen)
        (setq
         quakec--flymake-proc
         (make-process
          :name (format "quakec-flymake-%s-proc" compiler-exec) :noquery t :connection-type 'pipe
          :buffer (generate-new-buffer (format " *quakec-flymake-%s*" compiler-exec))
          :command command
          ;; make sure the file context is inherited, especially the
          ;; default-directory variable
          :file-handler t
          :sentinel
          (lambda (proc _event)
            (when (memq (process-status proc) '(exit signal))
              (unwind-protect
                  (if (with-current-buffer sourcebuf (eq proc quakec--flymake-proc))
                      (with-current-buffer (process-buffer proc)
                        (quakec--flymake-collect-diagnostics report-fn sourcebuf diag-re))
                    (flymake-log :warning "Canceling obsolete check %s" proc))
                (kill-buffer (process-buffer proc)))))))
        (process-send-eof quakec--flymake-proc)))))

(defvar quakec-flymake-fteqcc
  (funcall #'quakec--make-flymake-backend "fteqcc" quakec-flymake-fteqcc-cmd #'quakec--flymake-fteqcc-build-diagnostic-re))

(defvar quakec-flymake-gmqcc
  (funcall #'quakec--make-flymake-backend "gmqcc" quakec-flymake-gmqcc-cmd #'quakec--flymake-gmqcc-build-diagnostic-re))

;;;###autoload
(defun quakec-setup-flymake-fteqcc-backend ()
  "Setup a Flymake diagnostic function using FTEQCC."
  (add-hook 'flymake-diagnostic-functions quakec-flymake-fteqcc))

;;;###autoload
(defun quakec-setup-flymake-gmqcc-backend ()
  "Setup a Flymake diagnostic function using GMQCC."
  (add-hook 'flymake-diagnostic-functions quakec-flymake-gmqcc))


;;
;;; Compilation mode support
;;

(with-eval-after-load 'compile
  ;; compile warning highlighting for FTEQCC
  (add-to-list 'compilation-error-regexp-alist 'fteqcc)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(fteqcc "^\\(.*\\)(\\([0-9]+\\)):\\(.*\\)$" 1 2))

  ;; compiler warning highlighting for GMQCC
  (add-to-list 'compilation-error-regexp-alist 'gmqcc)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(gmqcc "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\): \\(.*\\)$" 1 2 3)))

;;
;;; Compile commands
;;

;;;###autoload
(defun quakec-compile (&optional _)
  "Compile a QuakeC project."
  (interactive)
  (let ((default-directory (quakec--find-project-root))
        (compile-cmd (compilation-read-command
                      quakec-compile-command)))
    (compile compile-cmd)))

;;;###autoload
(defun quakec-recompile ()
  "Recompile a QuakeC project."
  (interactive)
  (let ((default-directory (or compilation-directory
                               (quakec--find-project-root))))
    (recompile)))

;;
;;; Mode map
;;

(defvar quakec-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'quakec-compile)
    (define-key map (kbd "C-c C-r") 'quakec-recompile)
    map)
  "Keymap for QuakeC major mode.")

;; TODO: c-mode brings A LOT of cruft. E.g., some of the tweaks
;; introduced in the c-mode hook by users might not be relevant for
;; quakec-mode. Maybe worth reimplementing half of it (syntax,
;; indenting, etc) in a saner way.
;;
;;;###autoload
(define-derived-mode quakec-mode c-mode "QuakeC"
  "Major mode for editing QuakeC files.

\\{quakec-mode-map}"
  :group 'quakec

  ;; Basic syntax highlighting
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  ;; And font-locking
  (setq font-lock-defaults '((quakec--font-lock-keywords)))
  (setq-local font-lock-comment-face 'quakec-comment-face)

  ;; Imenu
  (setq imenu-case-fold-search t)
  (setq imenu-create-index-function #'quakec--imenu-create-index)

  ;; which-function support
  (add-hook 'which-func-functions #'quakec--which-func nil 'local)

  ;; Eldoc setup
  (add-hook 'after-save-hook #'quakec--after-save-hook nil 'local)
  (setq-local eldoc-documentation-function #'quakec--eldoc-function)
  (quakec--update-definitions)

  ;; Compile defaults setup
  (setq-local compile-command quakec-compile-command)

  ;; Completion backend
  (setq-local completion-at-point-functions '(quakec-completion-at-point))

  ;; Definition lookups through xref
  (setq-local xref-backend-functions #'quakec-xref-backend))

;;;###autoload
(define-generic-mode quakec-progs-mode
  '("//") ;; comments
  nil     ;; keywords
  `(;; pragmas first
    (,quakec--pragmas-re . quakec-preprocessor-face)

    ;; the output dat file on the first line
    ("\\`\\(\\([\\.[:alnum:]_]+/\\)*?\\)\\([[:alnum:]_]+\\.dat\\)"
     (1 quakec-progs-output-path-face)
     (3 quakec-progs-output-fname-face))

    ;; the rest - files to include
    ("^\\(\\([[:alnum:]_]+/\\)*?\\)\\([\\.[:alnum:]_]+\\.qc\\)"
     (1 quakec-progs-path-face)
     (3 quakec-progs-fname-face)))
  '("\\.src$") ;; auto-mode list
  nil          ;; functions to run
  "A mode for progs.src QuakeC files.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qc\\'" . quakec-mode))



(provide 'quakec-mode)

;;; quakec-mode.el ends here
