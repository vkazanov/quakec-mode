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

(defgroup quakec-mode nil
  "Support for QuakeC code."
  :link '(url-link "https://github.com/vkazanov/quakec-mode")
  :group 'languages)

(defcustom quakec-project-source "progs.src"
  "QuakeC project root file to use for location of a root
directory."
  :type 'string
  :group 'quakec-mode)


(defcustom quakec-compile-command quakec-fteqcc-compile-command
  "A default command to use for compiling QuakeC project."
  :type 'string
  :group 'quakec-mode)

(defcustom quakec-flymake-fteqcc-cmd '("fteqcc" "-Wall")
  "A list of strings to use as a command when running the FTEQCC
flymake backend"
  :type '(repeat string)
  :group 'quakec-mode)

(defcustom quakec-flymake-gmqcc-cmd '("gmqcc" "-Wall" "-nocolor" "-std=fteqcc")
  "A list of strings to use as a command when running the QMQCC
flymake backend"
  :type '(repeat string)
  :group 'quakec-mode)

;;
;;; Faces
;;

(defvar quakec-keyword-face 'quakec-keyword-face)
(defface quakec-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords"
  :group 'quakec-mode)

(defvar quakec-type-face 'quakec-type-face)
(defface quakec-type-face
  '((t :inherit font-lock-type-face))
  "Face for types"
  :group 'quakec-mode)

(defvar quakec-constant-face 'quakec-constant-face)
(defface quakec-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for constants"
  :group 'quakec-mode)

(defvar quakec-builtin-face 'quakec-builtin-face)
(defface quakec-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for builtins"
  :group 'quakec-mode)

(defvar quakec-preprocessor-face 'quakec-preprocessor-face)
(defface quakec-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor pragmas"
  :group 'quakec-mode)

(defvar quakec-variable-name-face 'quakec-variable-name-face)
(defface quakec-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variable names"
  :group 'quakec-mode)

(defvar quakec-function-name-face 'quakec-function-name-face)
(defface quakec-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names"
  :group 'quakec-mode)

;;; progs.src faces

(defvar quakec-progs-output-path-face 'font-lock-string-face)
(defface quakec-progs-output-path-face
  '((t :inherit font-lock-string-face))
  "Face for filenames in QuakeC progs.src files"
  :group 'quakec-mode)

(defvar quakec-progs-output-fname-face 'quakec-progs-output-fname-face)
(defface quakec-progs-output-fname-face
  '((t :inherit font-lock-keyword-face))
  "Face for filenames in QuakeC progs.src files"
  :group 'quakec-mode)

(defvar quakec-progs-path-face 'quakec-progs-path-face)
(defface quakec-progs-path-face
  '((t :inherit font-lock-string-face))
  "Face for paths in QuakeC progs.src files"
  :group 'quakec-mode)

(defvar quakec-progs-fname-face 'quakec-progs-fname-face)
(defface quakec-progs-fname-face
  '((t :inherit font-lock-string-face))
  "Face for paths in QuakeC progs.src files"
  :group 'quakec-mode)

;;
;;; Syntax highlighting and (limited) parsing regexps
;;

(defvar quakec--pragmas-re
  (regexp-opt
   '("$frame" "$framegroupstart" "$framegroupend" "$flags" "$base" "$cd" "$modelname" "$origin" "$scale" "$skin")
   'symbols)
  "A regexp matching pragmas.")

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
     "nextthink" "think" "touch" "use" "blocked")
   'symbols)
  "A regexp matching builtin functions.")

(defvar quakec--constants-re
  (regexp-opt '("TRUE" "FALSE") 'symbols)
  "A regexp matching constants.")

(defvar quakec--keywords-re
  (regexp-opt '("if" "else" "for" "do" "while" "switch" "case" "default" "break" "continue" "return" "local") 'symbols)
  "A regexp matching keywords.")

(defvar quakec--basic-type-re
  (regexp-opt '("void" "entity" "float" "vector" "string") 'symbols)
  "A regexp catching basic types.")

(defvar quakec--name-re
  (rx-to-string '(one-or-more (or word "_")))
  "A regexp catching symbol names.")


(defvar quakec--qc-function-parameter-left-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
		      ;; return type
		      (regexp ,quakec--basic-type-re)
		      (zero-or-more whitespace)

                      ;; parameter list start
                      "("))
  "A regexp catching the prefix of function declaration.")

(defvar quakec--qc-function-parameter-re
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

(defvar quakec--qc-function-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
		      ;; return type
		      (regexp ,quakec--basic-type-re)
		      (zero-or-more whitespace)

		      ;; parameter list
		      "(" (zero-or-more (regexp ".")) ")"
		      (zero-or-more whitespace)

		      ;; function name
		      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching a function name.")

(defvar quakec--qc-function-frame-params-re
  (rx-to-string `(seq
                  ;; a usual function as a prefix
                  (regexp ,quakec--qc-function-re)

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

(defvar quakec--qc-method-parameter-left-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
                      "."

		      ;; return type
		      (regexp ,quakec--basic-type-re)
		      (zero-or-more whitespace)

                      ;; parameter list start
                      "("))
  "A regexp catching the prefix of a method declaration, i.e.
something like \".void(\".")

(defvar quakec--qc-method-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
                      "."

		      ;; return type
		      (regexp ,quakec--basic-type-re)
		      (zero-or-more whitespace)

		      ;; parameter list
		      "(" (zero-or-more (regexp ".")) ")"
		      (zero-or-more whitespace)

		      ;; method name
		      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching a method name.")


(defvar quakec--global-variable-re
  (rx-to-string `(seq line-start
                      ;; global variable type
                      (regexp ,quakec--basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching a global variable declaration.")

(defvar quakec--local-variable-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
                      ;; optional local keyword
                      (opt "local")
                      (zero-or-more whitespace)

                      ;; global variable type
                      (regexp ,quakec--basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching a local variable declaration.")

(defvar quakec--qc-variable-name-re
  (rx-to-string `(seq (zero-or-more whitespace)
                      ;; optional delimieter
                      (zero-or-more whitespace)
                      (opt ",")
                      (zero-or-more whitespace)

                      ;; parameter name
                      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching variable names in a list of names defined.")

(defvar quakec--field-re
  (rx-to-string `(seq line-start (zero-or-more whitespace)
                      "."

                      ;; field type
                      (regexp ,quakec--basic-type-re)
                      (zero-or-more whitespace)

                      ;; variable name
                      (group-n 1 (regexp ,quakec--name-re))))
  "A regexp catching an entity field declaration.")

(defvar quakec--definitions-re
  (rx-to-string `(or (regexp ,quakec--qc-function-re)
                     (regexp ,quakec--qc-method-re)
                     (regexp ,quakec--global-variable-re)
                     (regexp ,quakec--field-re)))
  "A regexp catching all global definitions in a file.

Regexp group 1 should always be the name of the symbol.")

;;
;;; Syntax highlighting (font-lock)
;;

(defvar quakec--font-lock-keywords
  `((,quakec--keywords-re . 'quakec-keyword-face)
    (,quakec--basic-type-re . 'quakec-type-face)
    (,quakec--constants-re . 'quakec-constant-face)
    (,quakec--builtins-re . 'quakec-builtin-face)
    (,quakec--pragmas-re . 'quakec-preprocessor-face)
    (,quakec--global-variable-re (1 'quakec-variable-name-face)
                                 (,quakec--qc-variable-name-re nil nil (1 'quakec-variable-name-face)))
    (,quakec--local-variable-re (1 'quakec-variable-name-face)
                                (,quakec--qc-variable-name-re nil nil (1 'quakec-variable-name-face)))
    (,quakec--field-re (1 'quakec-variable-name-face)
                       (,quakec--qc-variable-name-re nil nil (1 'quakec-variable-name-face)))
    (,quakec--qc-function-re . (1 'quakec-function-name-face ))
    (,quakec--qc-method-re . (1 'quakec-function-name-face ))
    (,quakec--qc-function-parameter-left-re (,quakec--qc-function-parameter-re nil nil (1 'quakec-variable-name-face)))
    (,quakec--qc-method-parameter-left-re (,quakec--qc-function-parameter-re nil nil (1 'quakec-variable-name-face)))
    (,quakec--qc-function-frame-params-re (2 'quakec-variable-name-face)
                                          (3 'quakec-variable-name-face))))

;;
;;; Cached definition lookup
;;

;; TODO: to be reused across all defintion-based facilities
;;
(cl-defstruct (quakec--definition (:constructor nil)
                                  (:constructor quakec--definition-create (name beg end signature))
                                  (:copier nil))
  "A definition location and signature meant to be used in various
quakec-mode facilities relying on defition search."
  name beg end signature)

(defvar-local quakec--definitions-cache nil
  "A cache of QuakeC definitions in the current buffer.")

(defun quakec--update-definitions ()
  "Update the cache of QuakeC definitions."
  (setq quakec--definitions-cache (make-hash-table :test 'equal))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward quakec--definitions-re nil t)
      ;; not a comment
      (unless (nth 4 (syntax-ppss))
        (let* ((name (match-string-no-properties 1))
               (beg (match-beginning 0))
               (end (match-end 0))
               (signature (match-string 0))
               (def (quakec--definition-create name beg end signature)))
          (puthash name def quakec--definitions-cache))))))

(defun quakec--find-definition (name)
  "Retrieve a definition from definition cache by NAME."
  ;; try cache first, then update and try again
  (or (gethash name quakec--definitions-cache)
      (progn
        (quakec--update-definitions)
        (gethash name quakec--definitions-cache))))

;;
;;; Completion-at-point backend
;;

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
                       (while (re-search-forward quakec--qc-function-re nil t)
                         (unless(nth 4 (syntax-ppss))
                           (push (match-string-no-properties 1) completions)))
                       (goto-char (point-min))
                       (while (re-search-forward quakec--qc-method-re nil t)
                         (unless(nth 4 (syntax-ppss))
                           (push (match-string-no-properties 1) completions)))
                       (goto-char (point-min))
                       (while (re-search-forward quakec--global-variable-re nil t)
                         (unless(nth 4 (syntax-ppss))
                           (push (match-string-no-properties 1) completions)))
                       (goto-char (point-min))
                       (while (re-search-forward quakec--field-re nil t)
                         (unless(nth 4 (syntax-ppss))
                           (push (match-string-no-properties 1) completions)))
                       completions))))

      (list symbols-start symbols-end
            (completion-table-dynamic complete-function)
            :exclusive 'no))))

;;
;;; Xref (definitions lookup) support
;;

(defun quakec-xref-backend ()
  "File-local QuakeC Xref backend."
  'quakec)

(defun quakec--xref-find-definitions (identifier)
  "Find definitions of an IDENTIFIER in the current buffer."
  (let ((matches (list)))
    (when-let ((def (quakec--find-definition identifier))
               (point (quakec--definition-beg def)))
      (push (xref-make identifier (xref-make-buffer-location (current-buffer) point)) matches))
    matches))

(cl-defmethod xref-backend-definitions ((backend (eql quakec)) identifier)
  "QuakeC file-level definition finding Xref BACKEND.
Argument IDENTIFIER is a symbol to lookup."
  (quakec--xref-find-definitions identifier))

;;
;;; Imenu
;;

(defvar quakec--imenu-generic-expression
  `(("*Functions*" ,quakec--qc-function-re 1)
    ("*Methods*" ,quakec--qc-method-re 1)
    ("*Globals*" ,quakec--global-variable-re 1)
    ("*Fields*" ,quakec--field-re 1))
  "Imenu generic expression for `quakec-mode'.")

;;
;;; Eldoc
;;

(defun quakec--eldoc-function ()
  "Show a definition string for the current function or method at
point."
  (when-let*
      ((symbol-name (thing-at-point 'symbol 'no-props))
       (def (quakec--find-definition symbol-name)))
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
      (if (and (or (looking-at quakec--qc-function-re)
                   (looking-at quakec--qc-method-re)
                   (looking-at quakec--qc-method-re))
               (save-match-data
                 (end-of-defun)
                 (<= original-position (point))))
          (match-string-no-properties 1)
        nil))))

;;
;;; Project utils
;;

(defun quakec--find-project-root ()
  "Find the root of the QuakeC project, indicated by the
`quakec-project-source' (`progs.src') file. Signal a user error
if not in a project."
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               quakec-project-source)))
    (unless root
      (user-error "Not in a QuakeC project"))
    root))

(defun quakec--relative-path (&optional fpath)
  "Create a relative path for current buffer's file or FPATH with
respect to the project root."
  (unless fpath
    (setq fpath (buffer-file-name (current-buffer))))
  (file-relative-name fpath (quakec--find-project-root)))

;;
;;; On-the-fly syntax checking
;;


(defvar-local quakec--flymake-proc nil)

(defun quakec--flymake-fteqcc-build-diagnostic-re (fpath)
  (format "^\\(?:%s\\|-\\):\\([0-9]+\\): \\(.*\\)$" (regexp-quote fpath)))

(defun quakec--flymake-gmqcc-build-diagnostic-re (fpath)
  (format "^\\(?:%s\\|-\\):\\([0-9]+\\):[0-9]+: \\(.*\\)$" (regexp-quote fpath)))

(defun quakec--flymake-collect-diagnostics (report-fn sourcebuf diag-re)
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
  "Create a new flymake backend for a specific QuakeC compiler."
  (lambda (report-fn &rest _args)
    ;; Make sure a compiler is available
    (unless (executable-find
             compiler-exec) (error (format "Cannot find %s" compiler-exec)))

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
  (funcall 'quakec--make-flymake-backend "fteqcc" quakec-flymake-fteqcc-cmd 'quakec--flymake-fteqcc-build-diagnostic-re))

(defvar quakec-flymake-gmqcc
  (funcall 'quakec--make-flymake-backend "gmqcc" quakec-flymake-gmqcc-cmd 'quakec--flymake-gmqcc-build-diagnostic-re))

;;;###autoload
(defun quakec-setup-flymake-fteqcc-backend ()
  (add-hook 'flymake-diagnostic-functions quakec-flymake-fteqcc))

;;;###autoload
(defun quakec-setup-flymake-gmqcc-backend ()
  (add-hook 'flymake-diagnostic-functions quakec-flymake-gmqcc))


;;
;;; Compilation mode support
;;

(eval-after-load 'compile
  '(progn
     ;; compile warning highlighting for FTEQCC
     (add-to-list 'compilation-error-regexp-alist 'fteqcc)
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(fteqcc "^\\(.*\\)(\\([0-9]+\\)):\\(.*\\)$" 1 2))

     ;; compiler warning highlighting for GMQCC
     (add-to-list 'compilation-error-regexp-alist 'gmqcc)
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(gmqcc "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\): \\(.*\\)$" 1 2 3))))

;;
;;; Compile commands
;;

;;;###autoload
(defun quakec-compile (&optional args)
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
  :group 'quakec-mode

  ;; Basic syntax highlighting
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  ;; And font-locking
  (setq font-lock-defaults '((quakec--font-lock-keywords)))

  ;; Imenu
  (setq imenu-case-fold-search t)
  (setq imenu-generic-expression quakec--imenu-generic-expression)

  ;; which-function support
  (add-hook 'which-func-functions #'quakec--which-func nil 'local)

  ;; Eldoc setup
  (add-hook 'after-save-hook #'quakec--after-save-hook nil 'local)
  (setq-local eldoc-documentation-function #'quakec--eldoc-function)
  (quakec--update-definitions)

  ;; Compile defaults setup
  (setq-local compile-command quakec-compile-command))

;;;###autoload
(define-generic-mode quakec-progs-mode
  '("//") ;; comments
  nil     ;; keywords
  '( ;; the output dat file on the first line
    ("\\`\\(\\([\\.[:alnum:]]+/\\)*?\\)\\(\\w+\\.dat\\)"
     (1 'quakec-progs-output-path-face)
     (3 'quakec-progs-output-fname-face))
    ;; the rest - files to include
    ("^\\(\\(\\w+/\\)*?\\)\\(\\w+\\.qc\\)"
     (1 'quakec-progs-path-face)
     (3 'quakec-progs-fname-face)))
  '("\\.src$") ;; auto-mode list
  nil          ;; functions to run
  "A mode for progs.src QuakeC files")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qc\\'" . quakec-mode))



(provide 'quakec-mode)

;;; quakec-mode.el ends here
