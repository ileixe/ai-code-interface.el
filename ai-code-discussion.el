;;; ai-code-discussion.el --- AI code discussion operations -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides code discussion functionality for the AI Code Interface package.

;;; Code:

(require 'which-func)

(require 'ai-code-input)
(require 'ai-code-prompt-mode)

(declare-function ai-code-read-string "ai-code-input")
(declare-function ai-code--insert-prompt "ai-code-prompt-mode")

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
(defun ai-code-explain ()
  "Generate prompt to explain code at different levels.
If a region is selected, explain that specific region using function/file as context.
Otherwise, prompt user to select scope: symbol, line, function, or file.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive)
  (if (region-active-p)
      (ai-code--explain-region)
    (ai-code--explain-with-scope-selection)))

(defun ai-code--explain-region ()
  "Explain the selected region with function/file context."
  (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (function-name (which-function))
         (context-info (if function-name
                          (format "Function: %s" function-name)
                        ""))
         (initial-prompt (format "Please explain the following code:\n\n%s\n\n%s%s\nFile: %s\n\nProvide a clear explanation of what this code does, how it works, and its purpose within the context."
                        region-text
                        context-info
                        (if function-name "\n" "")
                        (or buffer-file-name "current buffer")))
         (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
    (when final-prompt
      (ai-code--insert-prompt final-prompt))))

(defun ai-code--explain-with-scope-selection ()
  "Prompt user to select explanation scope and explain accordingly."
  (let* ((choices '("symbol" "line" "function" "file"))
         (scope (completing-read "Select scope to explain: " choices nil t)))
    (pcase scope
      ("symbol" (ai-code--explain-symbol))
      ("line" (ai-code--explain-line))
      ("function" (ai-code--explain-function))
      ("file" (ai-code--explain-file)))))

(defun ai-code--explain-symbol ()
  "Explain the symbol at point."
  (let* ((symbol (thing-at-point 'symbol t))
         (function-name (which-function)))
    (unless symbol
      (user-error "No symbol at point"))
    (let* ((initial-prompt (format "Please explain the symbol '%s' in the context of:%s\nFile: %s\n\nExplain what this symbol represents, its type, purpose, and how it's used in this context."
                                  symbol
                                  (if function-name
                                      (format "\nFunction: %s" function-name)
                                    "")
                                  (or buffer-file-name "current buffer")))
           (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(defun ai-code--explain-line ()
  "Explain the current line."
  (let* ((line-text (string-trim (thing-at-point 'line t)))
         (line-number (line-number-at-pos))
         (function-name (which-function)))
    (let* ((initial-prompt (format "Please explain the following line of code:\n\nLine %d: %s\n\n%sFile: %s\n\nExplain what this line does, its purpose, and how it fits into the surrounding code."
                                  line-number
                                  line-text
                                  (if function-name
                                      (format "Function: %s\n" function-name)
                                    "")
                                  (or buffer-file-name "current buffer")))
           (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(defun ai-code--explain-function ()
  "Explain the current function."
  (let ((function-name (which-function)))
    (unless function-name
      (user-error "Not inside a function"))
    (let* ((initial-prompt (format "Please explain the function '%s':
File: %s
Explain what this function does, its parameters, return value, algorithm, and its role in the overall codebase."
                                  function-name
                                  (or buffer-file-name "current buffer")))
           (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))


(defun ai-code--explain-file ()
  "Explain the current file."
  (let ((file-name (or buffer-file-name "current buffer")))
    (let* ((initial-prompt (format "Please explain the following file:\nFile: %s\nProvide an overview of this file's purpose, its main components, key functions, and how it fits into the larger codebase architecture."
                                 file-name))
           (final-prompt (ai-code-read-string "Prompt: " initial-prompt)))
      (when final-prompt
        (ai-code--insert-prompt final-prompt)))))

(provide 'ai-code-discussion)

;;; ai-code-discussion.el ends here
