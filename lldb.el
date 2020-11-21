;; simple emacs interface for lldb
;; following interactive commands are provided:
;; lldb-run - run a program with lldb
;; lldb-attach - attach to a running process
;; lldb-set-breakpoint - set a breakpoint; list of breakpoints is stored in ~/.breakpoints
;; lldb-catch-throw - like gdb catch throw - stop debugger when c++ exception is thrown
;; lldb-catch-catch - like gdb catch catch - stop debugger when c++ exception is catched
;; lldb-list-breakpoints - show list of breakpoints
;; lldb-delete-breakpoint - delete breakpoint from lldb-list-breakpoints list
;; lldb-step-into
;; lldb-step-over
;; lldb-step-out
;; lldb-continue
;; lldb-eval-variable - evaluate value and show result in an outline-mode list
;; lldb-backtrace - show backtrace
;; variable output format changed to (template types removed): name = value (type)

;; TODO lldb improvements
;; show std::string more focused if no formatter is available
;; unique ptr - call get() to see the pointer value
;; add watch point at member attribute - stop debugger when value changes
;; watchpoint set expression -- 0x123456
;; # short form:
;; w s e -- 0x123456
;; variable refresh logic needed
;; eval with input: type in expression and evaluate

(require 'loop)
(require 'infix)

(defun lldb-insert-empty-line ()
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1)
  )

(defun lldb-show-arrow ()
  "show debug line marker"
  (setq overlay-arrow-position (make-marker))
  (set-marker overlay-arrow-position (line-beginning-position))
  ;; overlay - mark line
  (if (null (boundp 'lldb-ov-map))
      (setq lldb-ov-map (make-hash-table :test 'equal))
    )
  (let* ((ov-by-buffer (gethash (current-buffer) lldb-ov-map)))
    (if (null ov-by-buffer)
        (progn
          (setq ov-by-buffer (make-overlay (point-min) (point-min)))
          (overlay-put ov-by-buffer 'face 'bold) ;;'secondary-selection) ;; list-faces-display to get list of all colors with faces
          (puthash (current-buffer) ov-by-buffer lldb-ov-map)
          )
      )
    (move-overlay ov-by-buffer (line-beginning-position) (line-beginning-position 2))
    (setq lldb-last-arrow (list (current-buffer) (line-end-position))) ;; store last arrow position
    )
  )

(defun lldb-get-buffer-process ()
  (let ((res))
    (setq res (get-buffer-process "*lldb*"))
    (if (null res)
	(setq res (get-buffer-process "*lldb.exe*"))
      )
    (if (null res)
	(debug "lldb buffer for process not found")
	)
    res
    )
  )

(defun lldb-get-buffer ()
  (let ((res))
    (setq res (get-buffer "*lldb*"))
    (if (null res)
	(setq res (get-buffer "*lldb.exe*"))
	)
    (if (null res)
	(debug "lldb buffer not found")
	)
    res
    )
  )

(defun lldb-backtrace-find-file ()
  "switch frame pointer and open file in other window for backtrace entry at current point"
  (interactive)
  (save-excursion
    (if (search-forward-regexp "#\\([0-9]*\\)" (point-max) t)
        (progn
          (let ((no))
            (setq no (match-string 1))
            ;; set frame variable via "f <frame number>"
            (setq proc (lldb-get-buffer-process))
            (lldb-run-thing-process proc (concat "f " no "\n") t)
   ;;         (lldb-run-thing-process proc "bt\n" nil)
            )
          )
      )
    )
  )

(defun lldb-callback (arg)
  "hook to track current stack entry"
  (if (stringp arg)
      (progn
        (if (string-match "frame.* at \\(.*\\):\\([0-9]*\\)" arg)
            (progn
              ;; file name
              (let ((fn) (li) (buf))
                (setq buf (current-buffer))
                (setq fn (match-string 1 arg))
                (setq li (match-string 2 arg))
                (if (and li (file-exists-p fn))
                    (progn
                      (find-file-other-window fn)
                      ;; line
                      (goto-line (string-to-number li))
                      (lldb-show-arrow)
                      (switch-to-buffer-other-window buf)
                      ;; inline variable display is not working properly
                      ;;(lldb-set-timer-show-variable-inline)
                      )
                  )
                )
              )
          )
        )
    )
  )

(defun lldb-delete-overlay-values ()
  "delete the overlays showing the variable values"
  (interactive)
  (with-current-buffer (nth 0 lldb-last-arrow)
    (remove-overlays nil nil 'priority 2)
    )
  )

(defun lldb-do-overlay-value (name value)
  "show overlay value for given name"
  (let ((ov))
    (with-current-buffer (nth 0 lldb-last-arrow)
      (goto-char (nth 1 lldb-last-arrow))
      (if (search-backward name nil t)
          (progn
            (end-of-line)
            (setq ov (make-overlay (point) (point)))
            (overlay-put ov 'priority 2)
            (overlay-put ov 'before-string (propertize value 'face 'helm-source-header))
            ;; (move-overlay myo (point) (point))
            )
        )
      )
    )
  )

(defun lldb-strip-text-properties (text)
  (set-text-properties 0 (length text) nil text)
  text
  )

(defsubst lldb-string-join (strings &optional separator)
  (mapconcat #'identity strings separator)
  )

(defun lldb-show-variables ()
  "show variables via overlay for current two lines"
  (interactive)
  (setq lldb-timer nil)
  (let ((str) (proc) (start) (end) (name) (word) (line) (symbols))
    ;; get all words at current debugged line and line before
    (save-excursion ;; TODO (tm) extract to function
      (with-current-buffer (nth 0 lldb-last-arrow)
        (goto-char (nth 1 lldb-last-arrow))
        (setq line (line-number-at-pos))
        (setq symbols (list))
        (loop-while t
          (backward-word)
          (setq word (lldb-strip-text-properties (thing-at-point 'symbol)))
          (if (null (member word symbols))
              (push word symbols)
            )
          (if ($ (line-number-at-pos) + 1 < line)
              (loop-break)
            )
          )
        )
      (setq word (concat "fr v -o " (lldb-string-join symbols " ") "\n"))
      )
    (remove-hook 'comint-output-filter-functions 'lldb-callback t) ;; disable hook
    (setq proc (lldb-get-buffer-process))
    ;; "fr v" also shows uninitialized which i don't like
    ;;(setq str (lldb-run-thing-process proc "fr v\n" t))
    (setq str (lldb-run-thing-process proc word t))
    (add-hook 'comint-output-filter-functions 'lldb-callback) ;; and enable again
    (lldb-delete-overlay-values)
    (with-temp-buffer
      ;;(with-current-buffer (get-buffer-create "*hallo*")
      (insert str)
      (goto-char 0)
      ;; delete all content between { and }
      (loop-while (search-forward "{" nil t)
        (backward-char)
        (setq start (point))
        (forward-sexp)
        (setq end (point))
        (delete-region start end)
        (goto-char 0)
        )
      (goto-char 0)
      (flush-lines "^[*]*(lldb)$") ;; delete lines with lldb marker
      (goto-char 0)
      (loop-while (search-forward "(" nil t)
        (backward-char)
        (setq start (point))
        (forward-sexp)
        (setq end (point))
        (delete-region start end)
        (goto-char 0)
        )
      ;; this regex is not deleting the balanced parenthesis
      ;;(replace-regexp "([^)]*)[ ]*" "") ;; replace all types in complete buffer
      (loop-for-each-line
        (if (> (length it) 50)
            (setq it (substring it 0 50))
          )
        (if (string-match "\\([^ ]+\\) =" it)
            (progn
              (setq name (match-string 1 it))
              (lldb-do-overlay-value name it)
              )
          )
        )
      )
    )
  )

(defun lldb-set-timer-show-variable-inline ()
  (if (null (boundp 'lldb-timer))
      (setq lldb-timer nil)
    )
  (if lldb-timer
      (progn
        (cancel-timer lldb-timer)
        (setq lldb-timer nil)
        )
    )
  (setq lldb-timer (run-with-idle-timer 2 nil 'lldb-show-variables))
  )

;;(lldb-set-timer-show-variable-inline)

(defun lldb-step-into ()
  (interactive)
  (comint-send-string nil "s\n")
  )

(defun lldb-step-over ()
  (interactive)
  (comint-send-string nil "n\n")
  )

(defun lldb-step-out ()
  (interactive)
  (comint-send-string nil "finish\n")
  )

(defun lldb-continue ()
  (interactive)
  (lldb-delete-overlay-values)
  (if lldb-timer
      (progn
        (cancel-timer lldb-timer)
        (setq lldb-timer nil)
        )
    )
  (comint-send-string nil "c\n")
  )

(defun lldb-string-starts-with (str sub)
  (let ((res))
    (setq res (string-match sub str))
    (if res
	(if (= res 0)
	    t
	  ;;else
	  nil
	  )
      ;;else
      nil
      )
    )
  )

(defun lldb-run-thing-process (process command kill-result)
  "Send COMMAND to PROCESS"
  (let ((result) (last-chars) (start) (sub))
    (with-current-buffer (lldb-get-buffer)
      ;; do not delete complete buffer - do delete only the result
      ;;(erase-buffer)
      (end-of-buffer)
      (setq start (point))
      (comint-send-string process command)
      (if lldb-is-win ;; windows workaround - how to find (lldb) end marker?
	  (comint-send-string process "$\n")
	  )
      ;; wait until result is available
      (loop-while t
        (accept-process-output process 1)
        (progn
          (goto-char start)
          ;; Skip past the command, if it was echoed
          (setq sub (buffer-substring-no-properties start (+ start (length command))))
          (if (string-equal sub command)
              (progn
                (forward-line)
                (setq start (point))
                )
            ;;else
            (progn
              (setq sub (buffer-substring-no-properties start (+ start (length command) (length "(lldb) "))))
              (if (string-equal sub (concat "(lldb) " command))
                  (progn
                    (forward-line)
                    (setq start (point))
                    )
                )
              )
            )
          )
        (if (< (point-max) 7)
            (loop-continue)
          )
        (setq last-chars (buffer-substring-no-properties (- (point-max) 7) (- (point-max) 1)))
        (if (string-equal last-chars "(lldb)")
            (loop-break)
          )
	;; windows workaround - lldb does not print marker in case of error
	(if (and lldb-is-win (string-equal last-chars "mmand."))
	    (progn
	      (end-of-buffer)
	      (backward-char)
	      (kill-whole-line)
	      (loop-break)
	      )
	    )
        )
      (setq result (buffer-substring-no-properties start (- (point-max) 1)))
      ;;(debug result)
      (if kill-result
          (kill-region start (point-max))
        )
      result
      )
    )
  )

(defun lldb-backtrace ()
  (interactive)
  (remove-hook 'comint-output-filter-functions 'lldb-callback t) ;; disable hook
  (setq proc (lldb-get-buffer-process))
  (if proc
      (lldb-run-thing-process proc "bt\n" nil)
    )
  (add-hook 'comint-output-filter-functions 'lldb-callback) ;; and enable again
  (end-of-buffer)
  )

(defun lldb-current-prefix-with-asterisk ()
  (if (search-backward-regexp "^\\([*]*\\)" nil t)
      (match-string-no-properties 1)
    ;;else
    ""
    )
  )

(defun lldb-variable-type-at-end ()
  (interactive)
  (let ((start)(end) (str))
    (save-excursion
      (beginning-of-line)
      (if (search-forward-regexp "([^)]+)" nil t)
          (progn
            (setq start (match-beginning 0))
            (setq end (match-end 0))
            (setq str (buffer-substring start end))
            (delete-region start end)
            (end-of-line)
            (insert str)
            )
        )
      )
    )
  )

(defun lldb-extract-expr-result (str prefix)
  (with-temp-buffer
    (insert str)
    (move-types-to-end-of-lines)
    (remove-template-types)
    (goto-char 0) ;; jump to first position
    (replace-regexp "  " "*")
    (goto-char 0) ;; jump to first position
    (replace-regexp "= {" "")
    (goto-char 0)
    (flush-lines  "^[*]*}$") ;; delete lines matching regex
    (goto-char 0)
    (flush-lines "^[*]*(lldb)$") ;; delete lines with lldb marker
    (goto-char 0)
    (replace-regexp "^" "*") ;; insert one "*" at each line
    (goto-char 0)
    (replace-regexp "^" prefix) ;; insert prefix * at each line
    ;;(debug (buffer-string))
    (buffer-string)
    )
  )

(defun lldb-string-contains (string substr)
  (string-index-of string substr)
  )

(defun lldb-variable-expand-2 ()
  (interactive)
  (let ((str) (line-str) (path)(current-type))
    (setq path "")
    (setq line-str (thing-at-point 'line t))
    (save-excursion
      (if (string-match "([^)]+)" line-str)
          (progn
            (setq current-type (match-string 0 line-str)) 
            )
        )
      (loop-while t
        (if (string-match "\\([$a-zA-Z0-9_]+\\) \\(=\\|$\\)" line-str)
            (progn
              (setq str (match-string 1 line-str))
              (if (null (lldb-string-contains line-str (concat "(" str ")")))
                  (progn
                    (if (= (length path) 0)
                        (setq path str)
                      ;; else
                      (setq path (concat str "." path))
                      )
                    )
                )
              (if (lldb-string-starts-with str "\\$") ;; starts with dollar character
                  (progn
                    (loop-break)
                    )
                )
              (outline-up-heading 1)
              (setq line-str (thing-at-point 'line t))
              )
          ;; else
          (progn
            (loop-break)
            )
          )
        )
      )
    (if (length path)
        (progn
          ;;  do we have a pointer or reference?
          (if (lldb-string-contains current-type "*")
              (setq path (concat "*" path))
            )
          (end-of-line)
          (setq prefix (lldb-current-prefix-with-asterisk))
          (end-of-line)      
          (setq proc (lldb-get-buffer-process))
          (remove-hook 'comint-output-filter-functions 'lldb-callback t) ;; disable hook
          (setq str (lldb-run-thing-process proc (concat "expr -T -- " path "\n") t))
          (add-hook 'comint-output-filter-functions 'lldb-callback) ;; and enable again
          (setq result (lldb-extract-expr-result str prefix))
          (save-excursion
            (lldb-insert-empty-line)
            (insert result)
            )
          (outline-hide-subtree)
          (save-excursion
            (outline-show-children)
            (next-line)
            (outline-show-children)
            )
          (beginning-of-line)
          )
      )
    )
  )

(defun lldb-variable-expand ()
  (interactive)
  (let ((line-str) (prefix) (type) (addr) (str) (result))
    (setq line-str (thing-at-point 'line t))
    (if (string-match "[^=]*=[ ]*\\([xabcdef0-9]*\\)[ ]*\\(([^)]*)\\)" line-str)
        (progn
          (setq type (match-string 2 line-str))
          (setq addr  (match-string 1 line-str))
          (end-of-line)
          (setq prefix (lldb-current-prefix-with-asterisk))
          (end-of-line)      
          (setq proc (lldb-get-buffer-process))
          (remove-hook 'comint-output-filter-functions 'lldb-callback t) ;; disable hook
          (setq str (lldb-run-thing-process proc (concat "expr -T -- *(" type addr")\n") t))
	  (add-hook 'comint-output-filter-functions 'lldb-callback) ;; and enable again
          (setq result (lldb-extract-expr-result str prefix))
          (save-excursion
            (lldb-insert-empty-line)
            (insert result)
            )
          (outline-hide-subtree)
          (save-excursion
            (outline-show-children)
            (next-line)
            (outline-show-children)
            )
          (beginning-of-line)
          )
      )
    )
  )

(defun lldb-variable-plain-string ()
  (interactive)
;; settings set escape-non-printables false
  (let ((line-str) (prefix) (type) (addr) (str) (result))
    (setq line-str (thing-at-point 'line t))
    (if (string-match "\\(([^)]*)\\)[^=]*= \\(0x[0-9a-xA-X]*\\)" line-str)
        (progn
          (setq type (match-string 1 line-str))
          (setq addr  (match-string 2 line-str))
          (end-of-line)
          (setq prefix (lldb-current-prefix-with-asterisk))
          (end-of-line)      
          (setq proc (lldb-get-buffer-process))
          (remove-hook 'comint-output-filter-functions 'lldb-callback t) ;; disable hook
          (setq str (concat "p/s " type addr "\n"))
          ;;(debug str)
          (setq str (lldb-run-thing-process proc str t))
          (add-hook 'comint-output-filter-functions 'lldb-callback) ;; and enable again
          (setq result (with-temp-buffer
                         (insert str)
                         (goto-char 0)
                         (replace-regexp "^" prefix) ;; insert prefix * at each line
                         ;;(debug (buffer-string))
                         (buffer-string)
                         )
                )
          (save-excursion
            (lldb-insert-empty-line)
            (insert result)
            )
          (beginning-of-line)
          (outline-hide-subtree)
          (outline-show-children)
          )
      )
    )
  )

(defun move-types-to-end-of-lines ()
  "find (...) and move substring to end of line"
  (let ((start) (end) (sub))
    (goto-char 0)
    (loop-while (search-forward "(" nil t)
      (backward-char)
      (setq start (point))
      (forward-sexp)
      (setq end (point))
      (end-of-line)
      (if (not (eq (point) end))
          (progn
            (setq sub (buffer-substring-no-properties start end))
            (delete-region start end)
            (insert " ")
            (insert sub)
            )
          )
      )
    )
  )

(defun remove-template-types ()
  (c++-mode) ;; enable cpp mode
  (goto-char 0)
  (let ((start) (end))
    (loop-while (search-forward "<" nil t)
      (backward-char)
      (setq start (point))
      (forward-sexp)
      (setq end (point))
      (delete-region start end)
      )
    )
  (c++-mode) ;; disable cpp mode
  )

(defun lldb-eval-variable ()
  (interactive)
  (let ((word) (proc) (str) (result))
    (setq word (lldb-strip-text-properties (thing-at-point 'symbol)))
    (if (= (length word) 0)
        (setq word (completing-read "Expression:" (list word) nil nil word))
      )
    (setq proc (lldb-get-buffer-process))
    (remove-hook 'comint-output-filter-functions 'lldb-callback t) ;; disable hook
    (setq str (lldb-run-thing-process proc (concat "expr -T -- " word "\n") t))
    (add-hook 'comint-output-filter-functions 'lldb-callback) ;; and enable again
    (setq result (with-temp-buffer
                   (insert str)
                   (move-types-to-end-of-lines)
                   (remove-template-types)
                   (goto-char 0) ;; jump to first position
                   (replace-regexp "  " "*")
                   (goto-char 0) ;; jump to first position
                   (replace-regexp "= {" "")
                   (goto-char 0)
                   (flush-lines  "^[*]*}$") ;; delete lines matching regex
                   (goto-char 0)
                   (flush-lines "^[*]*(lldb)$") ;; delete lines with lldb marker
                   (goto-char 0)
                   (replace-regexp "^" "*") ;; insert one "*" at each line
                   (buffer-string)
                   )
                )
    (with-current-buffer (get-buffer-create "lldb-variable-tree")
      ;;(debug lines)
      ;;(erase-buffer)
      (end-of-buffer) ;; jump to very end of buffer
      (lldb-insert-empty-line)
      (lldb-insert-empty-line)
      (save-excursion
        (insert result)
        )
      ;; check if major-mode is set to outline-mode
      (if (null (eq major-mode 'outline-mode))
          (progn
            ;;(debug major-mode)
            (outline-mode)
          )
        )
      (outline-hide-subtree)
      (outline-show-children)
      ;;(outline-hide-sublevels 1)
      (display-buffer (current-buffer))
      (local-set-key (kbd "e") 'lldb-variable-expand)
      (local-set-key (kbd "M-e") 'lldb-variable-expand-2)
      (local-set-key (kbd "s") 'lldb-variable-plain-string) ;; show string unconverted
      (local-set-key (kbd "t") 'lldb-variable-type-at-end) ;; move type to end of line
      )
    )
;; expr -T -- <variable>
  )

(defun lldb-attach (working-directory pid)
  (interactive (list nil nil))
  (if (string-equal system-type "windows-nt")
      (setq lldb-is-win t)
    ;; else
    (setq lldb-is-win nil)
    )
  (if (null working-directory)
      (setq working-directory default-directory)
    )
  (if (null (boundp 'lldb-executable))
      (setq lldb-executable "lldb")
    )
  (if (null (file-exists-p lldb-executable))
      (setq lldb-executable (read-file-name "LLDB executable:"))
    )
  (if (null pid)
      (setq pid (read-string "PID:"))
      )
  (let ((default-directory working-directory) (word))
    (comint-run lldb-executable)
    ;; register lldb-callback only for local buffer
    (make-variable-buffer-local 'comint-output-filter-functions)
    (add-hook 'comint-output-filter-functions 'lldb-callback)
    ;; register keyboard shortcuts
    (local-set-key (kbd "<f3>") 'lldb-backtrace-find-file)
    (local-set-key (kbd "<f4>") 'lldb-backtrace)
    (local-set-key (kbd "<f5>") 'lldb-step-into)
    (local-set-key (kbd "<f6>") 'lldb-step-over)
    (local-set-key (kbd "<f7>") 'lldb-step-out)
    (local-set-key (kbd "<f8>") 'lldb-continue)
    (global-set-key (kbd "<f10>") 'lldb-eval-variable)
    ;; set full path config in llvm
    (comint-send-string nil "settings set frame-format frame #${frame.index}: { ${module.file.basename}{`${function.name-with-args}{${frame.no-debug}${function.pc-offset}}}}{ at ${line.file.fullpath}:${line.number}}{${function.is-optimized} [opt]}\\n\n")
    ;; dynamic types in lldb
;;    (comint-send-string nil "settings set target.prefer-dynamic-value run-target\n")
    (comint-send-string nil (concat "process attach --pid " pid "\n"))
    ;; read breakpoints
    (with-current-buffer (find-file-noselect "~/.breakpoints")
      (setq word (buffer-string))
      )
    (dolist (line (split-string word "\n"))
      (if (> (length line) 0)
          (comint-send-string nil (concat line "\n"))
        )
      )
    )
  )

(defun lldb-run (working-directory executable arguments)
  (interactive (list nil nil nil))
  (if (string-equal system-type "windows-nt")
      (setq lldb-is-win t)
    ;; else
    (setq lldb-is-win nil)
    )
  (if (null working-directory)
      (setq working-directory default-directory)
    )
  (if (null (boundp 'lldb-executable))
      (setq lldb-executable "lldb")
    )
  (if (null (file-exists-p lldb-executable))
      (setq lldb-executable (read-file-name "LLDB executable:"))
    )
  (if (or (null executable)
          (null (file-exists-p executable)))
      (setq executable (read-file-name "Executable to be debugged:"))
    )
  (if (null arguments)
      (setq arguments (read-string "Arguments:"))
      )
  (let ((default-directory working-directory) (word))
    ;; TODO duplicate code from lldb-attach - refactor out
    (comint-run lldb-executable)
    ;; register lldb-callback only for local buffer
    (make-variable-buffer-local 'comint-output-filter-functions)
    (add-hook 'comint-output-filter-functions 'lldb-callback)
    ;; register keyboard shortcuts
    (local-set-key (kbd "<f3>") 'lldb-backtrace-find-file)
    (local-set-key (kbd "<f4>") 'lldb-backtrace)
    (local-set-key (kbd "<f5>") 'lldb-step-into)
    (local-set-key (kbd "<f6>") 'lldb-step-over)
    (local-set-key (kbd "<f7>") 'lldb-step-out)
    (local-set-key (kbd "<f8>") 'lldb-continue)
    (global-set-key (kbd "<f10>") 'lldb-eval-variable)
    ;; set full path config in llvm
    (comint-send-string nil "settings set frame-format frame #${frame.index}: { ${module.file.basename}{`${function.name-with-args}{${frame.no-debug}${function.pc-offset}}}}{ at ${line.file.fullpath}:${line.number}}{${function.is-optimized} [opt]}\\n\n")
    ;; dynamic types in lldb
;;    (comint-send-string nil "settings set target.prefer-dynamic-value run-target\n")
    (comint-send-string nil (concat "file " executable "\n"))
    ;; read breakpoints
    (with-current-buffer (find-file-noselect "~/.breakpoints")
      (setq word (buffer-string))
      )
    (dolist (line (split-string word "\n"))
      (if (> (length line) 0)
          (comint-send-string nil (concat line "\n"))
        )
      )
    (if (null arguments)
        (comint-send-string nil "r\n") ;; run the program now
      (comint-send-string nil (concat "r " arguments "\n")) ;; run the program now
      )
    )
  )

(defun lldb-open-file-for-breakpoint ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((file-name)(line-num))
      (if (search-forward-regexp "\\([^ ]+\\):\\([0-9]+\\)" nil t)
          (progn
            (setq file-name (match-string-no-properties 1))
            (setq line-num (match-string-no-properties 2))
            (if (file-exists-p file-name)
                (progn
                  (find-file file-name)
                  (goto-line (string-to-number line-num))
                  )
              )
            )
        )
      )
    )
  )

(defun lldb-delete-breakpoint ()
  (interactive)
  (save-excursion
    (let ((buf) (file-name) (line-num) (proc) (str) (break-no))
      (beginning-of-line)
      (setq file-name nil)
      (setq line-num nil)
      (if (search-forward-regexp "\\([^ ]+\\):\\([0-9]+\\)" nil t)
          (progn
            (setq file-name (match-string-no-properties 1))
            (setq line-num (match-string-no-properties 2))
            )
        )
      (setq buf (lldb-get-buffer))
      ;; delete breakpoint immediately if *lldb* buffer is open 
      (if (and buf file-name)
          (progn
            (setq proc (lldb-get-buffer-process))
            (remove-hook 'comint-output-filter-functions 'lldb-callback t) ;; disable hook
            (setq str (lldb-run-thing-process proc "breakpoint list\n" nil))
            (if (string-match (concat "\\([0-9]*\\):[^']*'" file-name "', line = " line-num) str)
                (progn
                  (setq break-no (match-string-no-properties 1 str))
                  (setq str (lldb-run-thing-process proc (concat "breakpoint delete " break-no "\n") nil))
                  )
                )
            (add-hook 'comint-output-filter-functions 'lldb-callback) ;; and enable again
            )
        )
      )
    )
  (kill-whole-line)
  )

(defun lldb-catch-throw ()
  (interactive)
  (let ((buf))
    (setq buf (lldb-get-buffer))
    (if buf
        (with-current-buffer buf
          (comint-send-string nil "break set -E c++\n")
          )
      )
    )
  )

(defun lldb-catch-catch ()
  (interactive)
  (let ((buf))
    (setq buf (lldb-get-buffer))
    (if buf
        (with-current-buffer buf
          (comint-send-string nil "b __cxa_begin_catch\n") ;; b __cxa_throw - stop at throw
          )
      )
    )
  )

(defun lldb-list-breakpoints ()
  (interactive)
  (find-file "~/.breakpoints")
  (local-set-key (kbd "RET") 'lldb-open-file-for-breakpoint)
  (local-set-key (kbd "<deletechar>") 'lldb-delete-breakpoint)
  (local-set-key (kbd "d") 'lldb-delete-breakpoint)
  (message "press RET to open file at breakpoint")
  )

(defun lldb-set-breakpoint ()
  (interactive)
  (let ((name) (line) (buf))
    (setq name (buffer-file-name))
    (setq line (line-number-at-pos))
    (with-current-buffer (find-file-noselect "~/.breakpoints")
      (goto-char (point-max))
      (insert (format "\nb %s:%i" name line))
      (save-buffer)
      )
    (setq buf (lldb-get-buffer))
    (if buf
        (with-current-buffer buf
          (comint-send-string nil (concat "b " name ":" (number-to-string line) "\n"))
          )
      )
    )
  )
