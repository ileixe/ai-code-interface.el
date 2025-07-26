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
(require 'ai-code-change)
(require 'ai-code-discussion)

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

;;;###autoload
(defun ai-code-send-command ()
  "Read a prompt from the user and send it to the AI service."
  (interactive)
  (when-let ((prompt (ai-code-read-string "Send to AI: ")))
    (ai-code--insert-prompt prompt)))

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

(defvar ai-code-run-file-history nil
  "History list for ai-code-run-current-file commands.")

;;;###autoload
(defun ai-code-run-current-file ()
  "Generate command to run current script file (.py or .sh).
Let user modify the command before running it in a compile buffer.
Maintains a dedicated history list for this command."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (file-ext (when current-file (file-name-extension current-file)))
         (file-name (when current-file (file-name-nondirectory current-file)))
         (last-command (when ai-code-run-file-history (car ai-code-run-file-history)))
         (default-command (cond
                          ;; Check if current file is in the last run command
                          ((and last-command file-name 
                                (string-match-p (regexp-quote file-name) last-command))
                           last-command)
                          ;; Generate default command based on file extension
                          ((string= file-ext "py")
                           (format "python %s" file-name))
                          ((string= file-ext "sh")
                           (format "bash %s" file-name))
                          (t nil))))
    (unless current-file
      (user-error "Current buffer is not visiting a file"))
    (unless default-command
      (user-error "Current file is not a .py or .sh file"))
    (let ((command (read-string 
                   (format "Run command for %s: " file-name)
                   default-command
                   'ai-code-run-file-history)))
      (let ((default-directory (file-name-directory current-file)))
        (compile command)))))

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
    ("!" "Run Current File" ai-code-run-current-file)
    ]
   ])

;;;###autoload
(global-set-key (kbd "C-c a") #'ai-code-menu)

(provide 'ai-code-interface)

;;; ai-code-interface.el ends here
