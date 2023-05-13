;;; quakec-mode.el --- Major mode for QuakeC  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2023 Vladimir Kazanov

;; Author: Vladimir Kazanov
;; Version: 0.1
;; Keywords: games, languages
;; URL: https://github.com/vkazanov/quakec-mode
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; A simple major mode for editing QuakeC files.
;;
;; The dialect is Vanilla QuakeC as described in the QuakeC Reference
;; Manual. The mode provides imenu support, syntax highlighting,
;; which-func, a simple file-local xref backend for finding definition
;; and a completion-at-point function for local symbol completion
;; function. By default fteqcc is used as a compile command, with
;; support for the output format added to relevant compilation mode
;; variable.

;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'xref)

(defvar quakec-default-compile-command "fteqcc -Wall "
  "A default compile command for QuakeC.")

(defvar quakec-pragmas-re
  (regexp-opt
   '("$frame" "$framegroupstart" "$framegroupend" "$flags" "$base" "$cd" "$modelname" "$origin" "$scale" "$skin")
   'symbols)
  "A regexp matching pragmas.")

(defvar quakec-builtins-re
  (regexp-opt
   '(;; Basic math
     "anglemod" "ceil" "fabs" "floor" "ftos" "rint" "random"

     ;; Vector math
     "makevectors" "normalize" "vlen" "vectoangles" "vectoyaw" "vtos"

     ;; Collision
     "checkbottom" "checkpos" "pointcontents" "traceline"

     ;; Combat
     "aim" "checkclient" "particle"

     ;; Console
     "cvar_set" "dprint" "localcmd"

     ;; Debug
     "debug" "coredump" "eprint" "error" "objerror" "traceoff" "traceon"

     ;; Entity management
     "find" "findradius" "lightstyle" "makestatic" "nextent" "remove" "setmodel" "spawn"

     ;; Movement
     "ChangeYaw" "droptofloor" "movetogoal" "setorigin" "setsize" "walkmove"

     ;; Message
     "bprint" "centerprint" "sprint"

     ;; Network
     "WriteAngle" "WriteByte" "WriteChar" "WriteCoord" "WriteEntity" "WriteShort" "WriteString"
     "multicast"

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
     "self" "nextthink" "think" "touch" "use" "blocked")
   'symbols)
  "A regexp matching builtin functions.")

(defvar quakec-constants-re
  (regexp-opt '("TRUE" "FALSE") 'symbols)
  "A regexp matching constants.")

(defvar quakec-keywords-re
  (regexp-opt '("if" "else" "for" "do" "while" "switch" "case" "default" "break" "continue" "return" "local") 'symbols)
  "A regexp matching keywords.")

(defvar quakec-basic-type-re
  (regexp-opt '("void" "entity" "float" "vector" "string") 'symbols)
  "A regexp catching basic types.")

(defvar quakec-name-re
  (rx-to-string '(one-or-more (or word "_")))
  "A regexp catching symbol names.")


(defvar quakec-qc-function-parameter-left-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
		      ;; return type
		      (regexp ,quakec-basic-type-re)
		      (zero-or-more whitespace)

                      ;; parameter list start
                      "("))
  "A regexp catching the prefix of function declaration.")

(defvar quakec-qc-function-parameter-re
  (rx-to-string `(seq (zero-or-more whitespace)
                      ;; optional delimieter
                      (zero-or-more whitespace)
                      (opt ",")
                      (zero-or-more whitespace)

		      ;; type
		      (regexp ,quakec-basic-type-re)
		      (zero-or-more whitespace)

                      ;; parameter name
                      (group-n 1 (regexp ,quakec-name-re))))
  "A regexp catching a single function formal parameter.")

(defvar quakec-qc-function-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
		      ;; return type
		      (regexp ,quakec-basic-type-re)
		      (zero-or-more whitespace)

		      ;; parameter list
		      "(" (zero-or-more (regexp ".")) ")"
		      (zero-or-more whitespace)

		      ;; function name
		      (group-n 1 (regexp ,quakec-name-re))))
  "A regexp catching a function name.")

(defvar quakec-qc-function-frame-params-re
  (rx-to-string `(seq
                  ;; a usual function as a prefix
                  (regexp ,quakec-qc-function-re)

                  ;; followed by 2 additional parameters
                  (zero-or-more whitespace) "="

                  (zero-or-more whitespace) "["

                  (zero-or-more whitespace)
                  (group-n 2 (seq "$" (regexp ,quakec-name-re)))

                  (zero-or-more whitespace) ","

                  (zero-or-more whitespace)
                  (group-n 3 (regexp ,quakec-name-re))

                  (zero-or-more whitespace) "]"))
  "A regexp catching a frame function additional params.")

(defvar quakec-qc-method-parameter-left-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
                      "."

		      ;; return type
		      (regexp ,quakec-basic-type-re)
		      (zero-or-more whitespace)

                      ;; parameter list start
                      "("))
  "A regexp catching the prefix of a method declaration, i.e.
something like \".void(\".")

(defvar quakec-qc-method-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
                      "."

		      ;; return type
		      (regexp ,quakec-basic-type-re)
		      (zero-or-more whitespace)

		      ;; parameter list
		      "(" (zero-or-more (regexp ".")) ")"
		      (zero-or-more whitespace)

		      ;; method name
		      (group-n 1 (regexp ,quakec-name-re))))
  "A regexp catching a method name.")


(defvar quakec-global-variable-re
  (rx-to-string `(seq line-start
                      ;; global variable type
                      (regexp ,quakec-basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec-name-re))))
  "A regexp catching a global variable declaration.")

(defvar quakec-local-variable-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
                      ;; optional local keyword
                      (opt "local")
                      (zero-or-more whitespace)

                      ;; global variable type
                      (regexp ,quakec-basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec-name-re))))
  "A regexp catching a local variable declaration.")

(defvar quakec-qc-variable-name-re
  (rx-to-string `(seq (zero-or-more whitespace)
                      ;; optional delimieter
                      (zero-or-more whitespace)
                      (opt ",")
                      (zero-or-more whitespace)

                      ;; parameter name
                      (group-n 1 (regexp ,quakec-name-re))))
  "A regexp catching variable names in a list of names defined.")

(defvar quakec-field-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
                      "."

                      ;; field type
                      (regexp ,quakec-basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec-name-re))))
  "A regexp catching an entity field declaration.")

(defvar quakec-definitions-re
  (rx-to-string `(or (regexp ,quakec-qc-function-re)
                     (regexp ,quakec-qc-method-re)
                     (regexp ,quakec-global-variable-re)
                     (regexp ,quakec-field-re)))
  "A regexp catching all global definitions in a file.

Regexp group 1 should always be the name of the symbol.")

(defun quakec-completion-at-point ()
  "Provide completion for local QuakeC symbols."
  (let (symbol-bounds symbols-start symbols-end complete-function)
    (setq symbol-bounds (bounds-of-thing-at-point 'symbol))
    (when (and (setq symbols-start (car symbol-bounds))
               (setq symbols-end (cdr symbol-bounds)))
      (setq complete-function
            (lambda (_) (save-excursion
                     (let (completions)
                       (goto-char (point-min))
                       (while (re-search-forward quakec-qc-function-re nil t)
                         (when (not (nth 4 (syntax-ppss)))
                           (push (match-string-no-properties 1) completions)))
                       (goto-char (point-min))
                       (while (re-search-forward quakec-qc-method-re nil t)
                         (when (not (nth 4 (syntax-ppss)))
                           (push (match-string-no-properties 1) completions)))
                       (goto-char (point-min))
                       (while (re-search-forward quakec-global-variable-re nil t)
                         (when (not (nth 4 (syntax-ppss)))
                           (push (match-string-no-properties 1) completions)))
                       (goto-char (point-min))
                       (while (re-search-forward quakec-field-re nil t)
                         (when (not (nth 4 (syntax-ppss)))
                           (push (match-string-no-properties 1) completions)))
                       completions))))

      (list symbols-start symbols-end
            (completion-table-dynamic complete-function)
            :exclusive 'no))))

(defun quakec-xref-backend ()
  "File-local QuakeC Xref backend."
  'quakec)

(defun quakec--find-file-definitions (identifier)
  "Find definitions of an IDENTIFIER in the current buffer."
  (let ((matches (list)))
    (save-excursion
      (goto-char (point-min))
      ;; Search for the identifier in the buffer
      (while (search-forward identifier nil t)
        (let ((point (point)))
          (save-excursion
            (beginning-of-line)
            ;; Check if the identifier is a definition and the context is right
            (if (and
                 ;; not in a comment
                 (not (nth 4 (syntax-ppss)))
                 ;; this is a definition indeed
                 (looking-at quakec-definitions-re)
                 ;; definition identifier is correct
                 (equal (match-string-no-properties 1) identifier))
                ;; got it
                (push (xref-make identifier (xref-make-buffer-location (current-buffer) point)) matches)))))
      matches)))

(cl-defmethod xref-backend-definitions ((backend (eql quakec)) identifier)
  "QuakeC file-level definition finding Xref BACKEND.
Argument IDENTIFIER is a symbol to lookup."
  (quakec--find-file-definitions identifier))

(defvar quakec-imenu-generic-expression
  `(("*Functions*" ,quakec-qc-function-re 1)
    ("*Methods*" ,quakec-qc-method-re 1)
    ("*Globals*" ,quakec-global-variable-re 1)
    ("*Fields*" ,quakec-field-re 1))
  "Imenu generic expression for `quakec-mode'.")

(defvar quakec-mode-syntax-table nil "Syntax table for `quakec-mode'.")

(setq quakec-mode-syntax-table
      (let ((syntable (make-syntax-table)))
        ;; C-style comments (`//' and `/* ... */')
        (modify-syntax-entry ?/ ". 124b" syntable)
        (modify-syntax-entry ?* ". 23" syntable)
        (modify-syntax-entry ?\n "> b" syntable)

        ;; Strings
        (modify-syntax-entry ?\" "\"" syntable)

        syntable))

(defvar quakec-font-lock-keywords
  `((,quakec-keywords-re . font-lock-keyword-face)
    (,quakec-basic-type-re . font-lock-type-face)
    (,quakec-constants-re . font-lock-constant-face)
    (,quakec-builtins-re . font-lock-builtin-face)
    (,quakec-pragmas-re . font-lock-preprocessor-face)
    (,quakec-global-variable-re (1 font-lock-variable-name-face)
                                (,quakec-qc-variable-name-re nil nil (1 font-lock-variable-name-face)))
    (,quakec-local-variable-re (1 font-lock-variable-name-face)
                               (,quakec-qc-variable-name-re nil nil (1 font-lock-variable-name-face)))
    (,quakec-field-re (1 font-lock-variable-name-face)
                      (,quakec-qc-variable-name-re nil nil (1 font-lock-variable-name-face)))
    (,quakec-qc-function-re . (1 font-lock-function-name-face ))
    (,quakec-qc-method-re . (1 font-lock-function-name-face ))
    (,quakec-qc-function-parameter-left-re (,quakec-qc-function-parameter-re nil nil (1 font-lock-variable-name-face)))
    (,quakec-qc-method-parameter-left-re (,quakec-qc-function-parameter-re nil nil (1 font-lock-variable-name-face)))
    (,quakec-qc-function-frame-params-re (2 font-lock-variable-name-face)
                                         (3 font-lock-variable-name-face))))

(defvar-local quakec-definitions-cache nil
  "A cache of QuakeC definitions in the current buffer.")

(defun quakec-update-definitions ()
  "Update the cache of QuakeC definitions."
  (setq quakec-definitions-cache (make-hash-table :test 'equal))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward quakec-definitions-re nil t)
      (let ((name (match-string-no-properties 1))
            (signature (match-string 0)))
        (puthash name signature quakec-definitions-cache)))))

(defun quakec-eldoc-function ()
  "Show a definition string for the current function or method at
point."
  (let ((symbol-name (thing-at-point 'symbol 'no-props)))
    (when symbol-name
      ;; try cache first, then update and try again
      (or (gethash symbol-name quakec-definitions-cache)
          (progn
            (quakec-update-definitions)
            (gethash symbol-name quakec-definitions-cache))))))

(defun quakec-after-save-hook ()
  "Update QuakeC definitions cache after saving the file."
  (when (eq major-mode 'quakec-mode)
    (quakec-update-definitions)))

(defun quakec-which-func ()
  "Find the current function name in a QuakeC buffer."
  (save-excursion
    (beginning-of-defun)
    (when (looking-at quakec-qc-function-re)
      (match-string-no-properties 1))))

;;;###autoload
(define-derived-mode quakec-mode c-mode "QuakeC"
  "Major mode for editing QuakeC files."

  ;; Basic syntax highlighting
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local syntax-table quakec-mode-syntax-table)

  ;; And font-locking
  (setq font-lock-defaults '((quakec-font-lock-keywords)))

  ;; Imenu
  (setq imenu-case-fold-search t)
  (setq imenu-generic-expression quakec-imenu-generic-expression)

  ;; which-function support
  (add-hook 'which-func-functions #'quakec-which-func nil 'local)

  ;; compile support, only FTEQCC for now
  (setq-local compile-command quakec-default-compile-command)
  (add-to-list 'compilation-error-regexp-alist 'fteqcc)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(fteqcc "^\\(.*\\)(\\([0-9]+\\)):\\(.*\\)$" 1 2))

  ;; Eldoc setup
  (add-hook 'after-save-hook #'quakec-after-save-hook nil 'local)
  (setq-local eldoc-documentation-function #'quakec-eldoc-function)
  (quakec-update-definitions))

(provide 'quakec-mode)

;;; quakec-mode.el ends here
