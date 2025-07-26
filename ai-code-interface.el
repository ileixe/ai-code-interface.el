;;; ai-code-interface.el --- AI code interface for editing AI prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a major mode for editing AI prompt files.

;;; Code:

(require 'org)
(require 'which-func)
(require 'magit)
(require 'transient)
(require 'cl-lib)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)
(require 'ai-code-agile)
(require 'ai-code-git)
(require 'flycheck nil t)

(declare-function gptel-get-answer "gptel-assistant" (prompt))

(defalias 'ai-code-cli-start #'claude-code)
(defalias 'ai-code-cli-switch-to-buffer #'claude-code-switch-to-buffer)
(defalias 'ai-code-cli-send-command #'claude-code-send-command)


;;;###autoload
(defcustom ai-code-auto-send-to-ai t
  "Whether to automatically send prompts to Claude Code when inserting them.
If non-nil, call `claude-code-send-command` after inserting a prompt."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-use-gptel-headline nil
  "Whether to use GPTel to generate headlines for prompt sections.
If non-nil, call `gptel-get-answer` from gptel-assistant.el to generate
headlines instead of using the current time string."
  :type 'boolean
  :group 'ai-code)

;;;###autoload
(defcustom ai-code-prompt-suffix nil
  "Suffix text to append to prompts after a new line.
If non-nil, this text will be appended to the end of each prompt
with a newline separator."
  :type '(choice (const nil) string)
  :group 'ai-code)

(defun ai-code--is-comment-line (line)
  "Check if LINE is a comment line based on current buffer's comment syntax.
Returns non-nil if LINE starts with one or more comment characters,
ignoring leading whitespace."
  (when comment-start
    (let ((comment-str (string-trim-right comment-start)))
      (string-match-p (concat "^[ \t]*"
                              (regexp-quote comment-str)
                              "+")
                      (string-trim-left line)))))

;;;###autoload
(defun ai-code-code-change (arg)
  "Generate prompt to change code under cursor or in selected region.
With a prefix argument (\\[universal-argument]), prompt for a change without adding any context.
If a region is selected, change that specific region.
Otherwise, change the function under cursor.
If nothing is selected and no function context, prompts for general code change.
Inserts the prompt into the AI prompt file and optionally sends to AI.

Argument ARG is the prefix argument."
  (interactive "P")
  (if arg
      (let ((prompt (ai-code-read-string "Change code (no context): " "")))
        (ai-code--insert-prompt prompt))
    (unless buffer-file-name
      (user-error "Error: buffer-file-name must be available"))
    (let* ((function-name (which-function))
           (region-active (region-active-p))
           (region-text (when region-active
                          (buffer-substring-no-properties (region-beginning) (region-end))))
           (prompt-label
            (cond (region-active
                   (if function-name
                       (format "Change code in function %s: " function-name)
                     "Change selected code: "))
                  (function-name
                   (format "Change function %s: " function-name))
                  (t "Change code: ")))
           (initial-prompt (ai-code-read-string prompt-label ""))
           (final-prompt
            (concat initial-prompt
                    (when region-text (concat "\n" region-text))
                    (when function-name (format "\nFunction: %s" function-name))
                    (format "\nFile: %s" buffer-file-name)
                    "\nNote: Please make the code change described above.")))
      (ai-code--insert-prompt final-prompt))))

;;;###autoload
(defun ai-code-implement-todo ()
  "Generate prompt to implement TODO comments in current context.
If region is selected, implement that specific region.
If cursor is on a comment line, implement that specific comment.
If cursor is inside a function, implement comments for that function.
Otherwise implement comments for the entire current file."
  (interactive)
  (if (not buffer-file-name)
      (message "Error: buffer-file-name must be available")
    (let* ((current-line (string-trim (thing-at-point 'line t)))
           (is-comment (ai-code--is-comment-line current-line))
           (function-name (which-function))
           (function-context (if function-name
                                 (format "\nFunction: %s" function-name)
                               ""))
           (region-text (when (region-active-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))
           (initial-input
            (cond
             (region-text
              (format "Please implement this requirement comment block in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific block.%s\nFile: %s"
                      region-text function-context buffer-file-name))
             (is-comment
              (format "Please implement this requirement comment in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific comment.%s\nFile: %s"
                      current-line function-context buffer-file-name))
             (function-name
              (format "Please implement all TODO in-place in function '%s'. The TODO are TODO comments. Keep the existing code structure and only implement these marked items."
                      function-name))
             (t
              (format "Please implement all TODO in-place in file '%s'. The TODO are TODO comments. Keep the existing code structure and only implement these marked items."
                      (file-name-nondirectory buffer-file-name)))))
           (prompt (ai-code-read-string "TODO implementation instruction: " initial-input)))
      (ai-code--insert-prompt prompt))))

;;;###autoload
(defun ai-code-ask-question (arg)
  "Generate prompt to ask questions about specific code.
With a prefix argument (\\[universal-argument]), prompt for a question without adding any context.
If a region is selected, ask about that specific region.
If cursor is in a function, ask about that function.
Otherwise, ask a general question about the file.
Inserts the prompt into the AI prompt file and optionally sends to AI.

Argument ARG is the prefix argument."
  (interactive "P")
  (if arg
      (let ((question (ai-code-read-string "Ask question (no context): " "")))
        (ai-code--insert-prompt question))
    (let* ((function-name (which-function))
           (region-active (region-active-p))
           (region-text (when region-active
                          (buffer-substring-no-properties (region-beginning) (region-end))))
           (prompt-label
            (cond
             (region-active
              (if function-name
                  (format "Question about selected code in function %s: " function-name)
                "Question about selected code: "))
             (function-name
              (format "Question about function %s: " function-name))
             (t "General question: ")))
           (question (ai-code-read-string prompt-label ""))
           (final-prompt
            (concat question
                    (when region-text
                      (concat "\n" region-text))
                    (when function-name
                      (format "\nFunction: %s" function-name))
                    (when buffer-file-name
                      (format "\nFile: %s" buffer-file-name))
                    "\nNote: This is a question only - please do not modify the code.")))
      (ai-code--insert-prompt final-prompt))))

;;;###autoload
(defun ai-code-send-command ()
  "Read a prompt from the user and send it to the AI service."
  (interactive)
  (when-let ((prompt (ai-code-read-string "Send to AI: ")))
    (ai-code--insert-prompt prompt)))

;;;###autoload
(defun ai-code-investigate-exception (arg)
  "Generate prompt to investigate exceptions or errors in code.
With a prefix argument (\\[universal-argument]), prompt for investigation without adding any context.
If a region is selected, investigate that specific error or exception.
If cursor is in a function, investigate exceptions in that function.
Otherwise, investigate general exception handling in the file.
Inserts the prompt into the AI prompt file and optionally sends to AI.

Argument ARG is the prefix argument."
  (interactive "P")
  (if arg
      (let ((prompt (ai-code-read-string "Investigate exception (no context): " "")))
        (ai-code--insert-prompt prompt))
    (let* ((function-name (which-function))
           (region-active (region-active-p))
           (region-text (when region-active
                          (buffer-substring-no-properties (region-beginning) (region-end))))
           (prompt-label
            (cond
             (region-active
              (if function-name
                  (format "Investigate exception in function %s: " function-name)
                "Investigate selected exception: "))
             (function-name
              (format "Investigate exceptions in function %s: " function-name))
             (t "Investigate exceptions in code: ")))
           (initial-prompt (ai-code-read-string prompt-label
                                                (concat "Analyze this code for potential exceptions, "
                                                        "error conditions, and exception handling patterns. "
                                                        "Identify missing error handling, suggest improvements, "
                                                        "and explain how exceptions should be handled.")))
           (final-prompt
            (concat initial-prompt
                    (when region-text (concat "\n\nSelected code:\n" region-text))
                    (when function-name (format "\nFunction: %s" function-name))
                    (when buffer-file-name (format "\nFile: %s" buffer-file-name))
                    (concat "\n\nPlease focus on:\n"
                            "1. Potential exception sources and error conditions\n"
                            "2. Current exception handling patterns\n"
                            "3. Missing error handling opportunities\n"
                            "4. Best practices for exception handling in this context\n"
                            "5. Suggestions for improving error handling and debugging"))))
      (ai-code--insert-prompt final-prompt))))

;;;###autoload
(defun ai-code-copy-buffer-file-name-to-clipboard ()
  "Copy the current buffer's file path or selected text to clipboard.
If in a magit status buffer, copy the current branch name.
If in a dired buffer, copy the file at point or directory path.
If in a regular file buffer with selected text, copy text with file path.
Otherwise, copy the file path of the current buffer."
  (interactive)
  (let ((path-to-copy
         (cond
          ;; If current buffer is a magit status buffer
          ((eq major-mode 'magit-status-mode)
           (magit-get-current-branch))
          ;; If current buffer is a file, use existing logic
          ((buffer-file-name)
           (if (use-region-p)
               (format "%s in %s"
                       (buffer-substring-no-properties (region-beginning) (region-end))
                       (buffer-file-name))
             (buffer-file-name)))
          ;; If current buffer is a dired buffer
          ((eq major-mode 'dired-mode)
           (let ((file-at-point (ignore-errors (dired-get-file-for-visit))))
             (if file-at-point
                 ;; If there's a file under cursor, copy its full path
                 file-at-point
               ;; If no file under cursor, copy the dired directory path
               (dired-current-directory))))
          ;; For other buffer types, return nil
          (t nil))))
    (if path-to-copy
        (progn
          (kill-new path-to-copy)
          (message (format "copied %s to clipboard" path-to-copy)))
      (message "No file path available to copy"))))

;;;###autoload
(transient-define-prefix ai-code-menu ()
  "Transient menu for AI Code Interface interactive functions."
  ["AI Code Commands"
   ["AI CLI session"
    ("a" "Start AI CLI" ai-code-cli-start)
    ("z" "Switch to AI CLI" ai-code-cli-switch-to-buffer)
    ("p" "Open prompt file" ai-code-open-prompt-file)
    ("b" "Send prompt block to AI" ai-code-prompt-send-block)
    ]
   ["AI Code Actions"
    ("c" "Code change (C-u: global)" ai-code-code-change)
    ("i" "Implement TODO" ai-code-implement-todo)
    ("q" "Ask question (C-u: global)" ai-code-ask-question)
    ("<SPC>" "Send command to AI" ai-code-send-command)
    ]
   ["AI Agile Development"
    ("r" "Refactor Code"               ai-code-refactor-book-method)
    ("t" "Test Driven Development"     ai-code-tdd-cycle)
    ("v" "Pull or Review Code Change"  ai-code-pull-or-review-diff-file)
    ]
   ["Other Tools"
    ("e" "Investigate exception (C-u: global)" ai-code-investigate-exception)
    ("f" "Fix Flycheck errors in scope" ai-code-flycheck-fix-errors-in-scope)
    ("k" "Copy Buffer File Name" ai-code-copy-buffer-file-name-to-clipboard)
    ]
   ])

;;;###autoload
(global-set-key (kbd "C-c p") #'ai-code-menu)

;;; Flycheck integration
(defun ai-code-flycheck--get-errors-in-scope (start end)
  "Return a list of Flycheck errors within the given START and END buffer positions."
  (when (and (bound-and-true-p flycheck-mode) flycheck-current-errors)
    (cl-remove-if-not
     (lambda (err)
       (let ((pos (flycheck-error-pos err)))
         (and (integerp pos) (>= pos start) (< pos end))))
     flycheck-current-errors)))

(defun ai-code-flycheck--format-error-list (errors file-path-for-error-reporting)
  "Formats a list string for multiple Flycheck ERRORS.
FILE-PATH-FOR-ERROR-REPORTING is the relative file path
to include in each error report."
  (let ((error-reports '()))
    (dolist (err errors)
      (let* ((line (flycheck-error-line err))
             (col (flycheck-error-column err))
             (msg (flycheck-error-message err)))
        (if (and (integerp line) (integerp col))
            (let* ((error-line-text
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
              (push (format "File: %s:%d:%d\nError: %s\nContext line:\n%s"
                            file-path-for-error-reporting line col msg error-line-text)
                    error-reports))
          (progn
            (message "AI-Code: Flycheck error for %s. Line: %S, Col: %S. Full location/context not available. Sending general error info."
                     file-path-for-error-reporting line col)
            (push (format "File: %s (Location: Line %s, Column %s)\nError: %s"
                          file-path-for-error-reporting
                          (if (integerp line) (format "%d" line) "N/A")
                          (if (integerp col) (format "%d" col) "N/A")
                          msg)
                  error-reports)))))
    (mapconcat #'identity (nreverse error-reports) "\n\n")))

(defun ai-code--choose-flycheck-scope ()
  "Return a list (START END DESCRIPTION) for Flycheck fixing scope."
  (let* ((scope (if (region-active-p) 'region
                  (intern
                   (completing-read
                    "Select Flycheck fixing scope: "
                    (delq nil
                          `("current-line"
                            ,(when (which-function) "current-function")
                            "whole-file"))
                    nil t))))
         start end description)
    (pcase scope
      ('region
       (setq start (region-beginning)
             end   (region-end)
             description
             (format "the selected region (lines %d–%d)"
                     (line-number-at-pos start)
                     (line-number-at-pos end))))
      ('current-line
       (setq start            (line-beginning-position)
             end              (line-end-position)
             description       (format "current line (%d)"
                                       (line-number-at-pos (point)))))
      ('current-function
       (let ((bounds (bounds-of-thing-at-point 'defun)))
         (unless bounds
           (user-error "Not inside a function; cannot select current function"))
         (setq start            (car bounds)
               end              (cdr bounds)
               description       (format "function '%s' (lines %d–%d)"
                                        (which-function)
                                        (line-number-at-pos (car bounds))
                                        (line-number-at-pos (cdr bounds))))))
      ('whole-file
       (setq start            (point-min)
             end              (point-max)
             description       "the entire file"))
      (_
       (user-error "Unknown Flycheck scope %s" scope)))
    (list start end description)))

;;;###autoload
(defun ai-code-flycheck-fix-errors-in-scope ()
  "Ask AI to generate a patch fixing Flycheck errors.
If a region is active, operate on that region.
Otherwise prompt to choose scope: current line, current function (if any),
or whole file.  Requires the `flycheck` package to be installed and available."
  (interactive)
  (unless (featurep 'flycheck)
    (user-error "Flycheck package not found.  This feature is unavailable"))
  (unless buffer-file-name
    (user-error "Error: buffer-file-name must be available"))
  (when (bound-and-true-p flycheck-mode)
    (if (null flycheck-current-errors)
        (message "No Flycheck errors found in the current buffer.")
      (let* ((git-root (or (magit-toplevel) default-directory))
             (rel-file (file-relative-name buffer-file-name git-root))
             ;; determine start/end/scope-description via helper
             (scope-data (ai-code--choose-flycheck-scope))
             (start (nth 0 scope-data))
             (end (nth 1 scope-data))
             (scope-description (nth 2 scope-data)))
        ;; collect errors and bail if none in that scope
        (let ((errors-in-scope
               (ai-code-flycheck--get-errors-in-scope start end)))
          (if (null errors-in-scope)
              (message "No Flycheck errors found in %s." scope-description)
            (let* ((error-list-string
                    (ai-code-flycheck--format-error-list errors-in-scope
                                                         rel-file))
                   (prompt
                    (if (string-equal "the entire file" scope-description)
                        (format (concat "Please fix the following Flycheck "
                                        "errors in file %s:\n\n%s\n\nFile: %s\n"
                                        "Note: Please make the code change "
                                        "described above.")
                                rel-file error-list-string buffer-file-name)
                      (format (concat "Please fix the following Flycheck "
                                      "errors in %s of file %s:\n\n%s\n\n"
                                      "File: %s\nNote: Please make the code "
                                      "change described above.")
                              scope-description
                              rel-file
                              error-list-string
                              buffer-file-name)))
                   (edited-prompt (ai-code-read-string "Edit prompt for AI: "
                                                       prompt)))
              (when (and edited-prompt (not (string-blank-p edited-prompt)))
                (ai-code--insert-prompt edited-prompt)
                (message "Generated prompt to fix %d Flycheck error(s) in %s."
                         (length errors-in-scope)
                         scope-description)))))))))

(provide 'ai-code-interface)

;;; ai-code-interface.el ends here
