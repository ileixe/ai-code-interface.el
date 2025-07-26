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

(require 'ai-code-input)
(require 'ai-code-prompt-mode)
(require 'ai-code-agile)
(require 'ai-code-git)

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
(defun ai-code-code-change (prefix-arg)
  "Generate prompt to change code under cursor or in selected region.
With a prefix argument (C-u), prompt for a change without adding any context.
If a region is selected, change that specific region.
Otherwise, change the function under cursor.
If nothing is selected and no function context, prompts for general code change.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive "P")
  (if prefix-arg
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
(defun ai-code-ask-question (prefix-arg)
  "Generate prompt to ask questions about specific code.
With a prefix argument (C-u), prompt for a question without adding any context.
If a region is selected, ask about that specific region.
If cursor is in a function, ask about that function.
Otherwise, ask a general question about the file.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive "P")
  (if prefix-arg
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
(defun ai-code-investigate-exception (prefix-arg)
  "Generate prompt to investigate exceptions or errors in code.
With a prefix argument (C-u), prompt for investigation without adding any context.
If a region is selected, investigate that specific error or exception.
If cursor is in a function, investigate exceptions in that function.
Otherwise, investigate general exception handling in the file.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive "P")
  (if prefix-arg
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
                                                "Analyze this code for potential exceptions, error conditions, and exception handling patterns. Identify missing error handling, suggest improvements, and explain how exceptions should be handled."))
           (final-prompt
            (concat initial-prompt
                    (when region-text (concat "\n\nSelected code:\n" region-text))
                    (when function-name (format "\nFunction: %s" function-name))
                    (when buffer-file-name (format "\nFile: %s" buffer-file-name))
                    "\n\nPlease focus on:\n1. Potential exception sources and error conditions\n2. Current exception handling patterns\n3. Missing error handling opportunities\n4. Best practices for exception handling in this context\n5. Suggestions for improving error handling and debugging")))
      (ai-code--insert-prompt final-prompt))))

;;;###autoload
(defun ai-code-copy-buffer-file-name-to-clipboard ()
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
    ("k" "Copy Buffer File Name" ai-code-copy-buffer-file-name-to-clipboard)
    ]
   ])

;;;###autoload
(global-set-key (kbd "C-c p") #'ai-code-menu)

(provide 'ai-code-interface)

;;; ai-code-interface.el ends here
