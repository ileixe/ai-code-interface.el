;;; ai-prompt-mode.el --- AI prompt mode for editing AI prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a major mode for editing AI prompt files.

;;; Code:

(require 'org)
(require 'which-func)
(require 'magit)
(require 'transient)

(defvar yas-snippet-dirs)

(declare-function yas-load-directory "yasnippet" (dir))
(declare-function yas-minor-mode "yasnippet")
(declare-function aider-read-string "aider-core" (prompt &optional initial-input candidate-list))
(declare-function claude-code-send-command "claude-code")
(declare-function claude-code-switch-to-buffer "claude-code")
(declare-function gptel-get-answer "gptel-assistant" (prompt))

;;;###autoload
(defcustom ai-prompt-file-name ".ai.prompt.org"
  "File name that will automatically enable `ai-prompt-mode` when opened.
This is the file name without path."
  :type 'string
  :group 'ai-prompt)

;;;###autoload
(defcustom ai-prompt-auto-send-to-ai t
  "Whether to automatically send prompts to Claude Code when inserting them.
If non-nil, call `claude-code-send-command` after inserting a prompt."
  :type 'boolean
  :group 'ai-prompt)

;;;###autoload
(defcustom ai-prompt-use-gptel-headline nil
  "Whether to use GPTel to generate headlines for prompt sections.
If non-nil, call `gptel-get-answer` from gptel-assistant.el to generate
headlines instead of using the current time string."
  :type 'boolean
  :group 'ai-prompt)

;;;###autoload
(defcustom ai-prompt-suffix nil
  "Suffix text to append to prompts after a new line.
If non-nil, this text will be appended to the end of each prompt
with a newline separator."
  :type '(choice (const nil) string)
  :group 'ai-prompt)

;;;###autoload
(defcustom ai-cli-type 'claude-code
  "Type of AI CLI service to use.
Possible values are 'claude-code (default) or 'gemini-cli."
  :type '(choice (const claude-code) (const gemini-cli))
  :group 'ai-prompt)

;;;###autoload
(defun ai-prompt-open-prompt-file ()
  "Open AI prompt file under git repo root.
If file doesn't exist, create it with sample prompt."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (prompt-file (when git-root
                        (expand-file-name ai-prompt-file-name git-root))))
    (if prompt-file
        (progn
          (find-file-other-window prompt-file)
          (unless (file-exists-p prompt-file)
            ;; Insert initial content for new file
            (insert "# AI Prompt File\n")
            (insert "# This file is for storing AI prompts and instructions\n")
            (insert "# Use this file to save reusable prompts for your AI assistant\n\n")
            (insert "* Sample prompt:\n\n")
            (insert "Explain the architecture of this codebase\n")
            (save-buffer)))
      (message "Not in a git repository"))))

(defun ai-prompt--setup-snippets ()
  "Setup YASnippet directories for `ai-prompt-mode`."
  (condition-case nil
      (when (require 'yasnippet nil t)
        (let ((snippet-dir (expand-file-name "snippets"
                                             (file-name-directory (file-truename (locate-library "ai-prompt-mode"))))))
          (when (file-directory-p snippet-dir)
            (unless (boundp 'yas-snippet-dirs)
              (setq yas-snippet-dirs nil))
            (add-to-list 'yas-snippet-dirs snippet-dir t)
            (ignore-errors (yas-load-directory snippet-dir))))
    (error nil)))) ;; Suppress all errors

(defun ai-prompt--get-ai-prompt-file-path ()
  "Get the path to the AI prompt file in the current git repository."
  (let* ((git-root (magit-toplevel)))
    (when git-root
      (expand-file-name ai-prompt-file-name git-root))))

(defun ai-prompt--insert-prompt (prompt-text)
  "Insert PROMPT-TEXT into the AI prompt file."
  (let ((prompt-file (ai-prompt--get-ai-prompt-file-path)))
    (if prompt-file
        (let ((buffer (if ai-prompt-auto-send-to-ai
                          (find-file-noselect prompt-file)
                        (find-file-other-window prompt-file))))
          (with-current-buffer buffer
            (goto-char (point-max))
            (unless (bolp)
              (insert "\n"))
            (insert "\n")
            (insert "** ")
            (if ai-prompt-use-gptel-headline
                (condition-case nil
                    (let ((headline (gptel-get-answer (concat "Create a 5-10 word action-oriented headline for this AI prompt that captures the main task. Use keywords like: refactor, implement, fix, optimize, analyze, document, test, review, enhance, add, remove, improve, integrate. Example: 'Optimize database queries' or 'Implement error handling'.\n\nPrompt: " prompt-text))))
                      (insert headline " ")
                      (org-insert-time-stamp (current-time) t t))
                  (error (org-insert-time-stamp (current-time) t t)))
              (org-insert-time-stamp (current-time) t t))
            (insert "\n")
            (let ((full-prompt (if ai-prompt-suffix
                                   (concat prompt-text "\n" ai-prompt-suffix "\n")
                                 prompt-text)))
              (insert full-prompt)
              (unless (bolp)
                (insert "\n"))
              (save-buffer)
              (message "Prompt added to %s" prompt-file)
              (when ai-prompt-auto-send-to-ai
                (ignore-errors (ai-cli-send-command full-prompt))
                (ai-cli-switch-to-buffer)))))
      (message "Not in a git repository"))))

(defun ai-cli-start ()
  "Start the AI CLI service based on `ai-cli-type`."
  (interactive)
  (cond
   ((eq ai-cli-type 'claude-code) (claude-code))
   ((eq ai-cli-type 'gemini-cli) (gemini-cli))
   (t (claude-code))))

(defun ai-cli-switch-to-buffer ()
  "Switch to the AI CLI buffer based on `ai-cli-type`."
  (interactive)
  (cond
   ((eq ai-cli-type 'claude-code) (claude-code-switch-to-buffer))
   ((eq ai-cli-type 'gemini-cli) (gemini-cli-switch-to-buffer))
   (t (claude-code-switch-to-buffer))))

(defun ai-cli-send-command (prompt-text)
  "Send PROMPT-TEXT to the AI service based on `ai-cli-type`."
  (cond
   ((eq ai-cli-type 'claude-code) (claude-code-send-command prompt-text))
   ((eq ai-cli-type 'gemini-cli) (gemini-cli-send-command prompt-text))
   (t (claude-code-send-command prompt-text))))

(defun ai-prompt--is-comment-line (line)
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
(defun ai-prompt-code-change (prefix-arg)
  "Generate prompt to change code under cursor or in selected region.
With a prefix argument (C-u), prompt for a change without adding any context.
If a region is selected, change that specific region.
Otherwise, change the function under cursor.
If nothing is selected and no function context, prompts for general code change.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive "P")
  (if prefix-arg
      (let ((prompt (aider-read-string "Change code (no context): " "")))
        (ai-prompt--insert-prompt prompt))
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
           (initial-prompt (aider-read-string prompt-label ""))
           (final-prompt
            (concat initial-prompt
                    (when region-text (concat "\n" region-text))
                    (when function-name (format "\nFunction: %s" function-name))
                    (format "\nFile: %s" buffer-file-name)
                    "\nNote: Please make the code change described above.")))
      (ai-prompt--insert-prompt final-prompt))))


;;;###autoload
(defun ai-prompt-implement-todo ()
  "Generate prompt to implement TODO comments in current context.
If region is selected, implement that specific region.
If cursor is on a comment line, implement that specific comment.
If cursor is inside a function, implement comments for that function.
Otherwise implement comments for the entire current file."
  (interactive)
  (if (not buffer-file-name)
      (message "Error: buffer-file-name must be available")
    (let* ((current-line (string-trim (thing-at-point 'line t)))
           (is-comment (ai-prompt--is-comment-line current-line))
           (function-name (which-function))
           (region-text (when (region-active-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))
           (initial-input
            (cond
             (region-text
              (format "Please implement this code block in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific block.\nFunction: %s\nFile: %s"
                      region-text function-name buffer-file-name))
             (is-comment
              (format "Please implement this comment in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific comment.\nFunction: %s\nFile: %s"
                      current-line function-name buffer-file-name))
             (function-name
              (format "Please implement all TODO in-place in function '%s'. The TODO are TODO comments. Keep the existing code structure and only implement these marked items."
                      function-name))
             (t
              (format "Please implement all TODO in-place in file '%s'. The TODO are TODO comments. Keep the existing code structure and only implement these marked items."
                      (file-name-nondirectory buffer-file-name)))))
           (prompt (aider-read-string "TODO implementation instruction: " initial-input)))
      (ai-prompt--insert-prompt prompt))))

;;;###autoload
(defun ai-prompt-ask-question (prefix-arg)
  "Generate prompt to ask questions about specific code.
With a prefix argument (C-u), prompt for a question without adding any context.
If a region is selected, ask about that specific region.
If cursor is in a function, ask about that function.
Otherwise, ask a general question about the file.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive "P")
  (if prefix-arg
      (let ((question (aider-read-string "Ask question (no context): " "")))
        (ai-prompt--insert-prompt question))
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
           (question (aider-read-string prompt-label ""))
           (final-prompt
            (concat question
                    (when region-text
                      (concat "\n" region-text))
                    (when function-name
                      (format "\nFunction: %s" function-name))
                    (when buffer-file-name
                      (format "\nFile: %s" buffer-file-name))
                    "\nNote: This is a question only - please do not modify the code.")))
      (ai-prompt--insert-prompt final-prompt))))

;; Define the AI Prompt Mode (derived from org-mode)
;;;###autoload
(define-derived-mode ai-prompt-mode org-mode "AI Prompt"
  "Major mode derived from `org-mode` for editing AI prompt files.
Special commands:
\{ai-prompt-mode-map}"
  ;; Basic setup
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local truncate-lines nil)  ; Disable line truncation, allowing lines to wrap
  (define-key ai-prompt-mode-map (kbd "C-c C-c") #'ai-prompt-send-block)
  ;; YASnippet support
  (when (require 'yasnippet nil t)
    (yas-minor-mode 1)
    (ai-prompt--setup-snippets)))

;;;###autoload
(defun ai-prompt-send-block ()
  "Send the current text block (paragraph) to the AI service.
The block is the text separated by blank lines. It trims leading/trailing whitespace."
  (interactive)
  (let* ((block-text (thing-at-point 'paragraph))
         (trimmed-text (when block-text (string-trim block-text))))
    (if (and trimmed-text (string-match-p "\\S-" trimmed-text))
        (progn
          (ai-cli-send-command trimmed-text)
          (ai-cli-switch-to-buffer))
      (message "No text in the current block to send."))))

;;;###autoload
(defun ai-prompt-send-command ()
  "Read a prompt from the user and send it to the AI service."
  (interactive)
  (when-let ((prompt (aider-read-string "Send to AI: ")))
    (ai-prompt--insert-prompt prompt)))

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(concat "/" (regexp-quote ai-prompt-file-name) "\'") . ai-prompt-mode))

;;;###autoload
(transient-define-prefix ai-prompt-menu ()
  "Transient menu for AI Prompt Mode interactive functions."
  ["AI Prompt Commands"
   ["AI CLI session"
    ("!" "Start AI CLI" ai-cli-start)
    ("z" "Switch to AI CLI" ai-cli-switch-to-buffer)
    ("p" "Open prompt file" ai-prompt-open-prompt-file)
    ("<SPC>" "Send command to AI" ai-prompt-send-command)
    ]
   ["AI Prompt Actions"
    ("c" "Code change (C-u: global)" ai-prompt-code-change)
    ("i" "Implement TODO" ai-prompt-implement-todo)
    ("q" "Ask question (C-u: global)" ai-prompt-ask-question)
    ("b" "Send prompt block to AI" ai-prompt-send-block)
    ]
   ])

;;;###autoload
(global-set-key (kbd "C-c p") #'ai-prompt-menu)

(provide 'ai-prompt-mode)

;;; ai-prompt-mode.el ends here
