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

(provide 'ai-code-discussion)

;;; ai-code-discussion.el ends here
