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
            (insert "** " (format-time-string "%Y-%m-%d %H:%M:%S") "\n")
            (insert prompt-text)
            (unless (bolp)
              (insert "\n"))
            (save-buffer)
            (message "Prompt added to %s" prompt-file)
            (when ai-prompt-auto-send-to-ai
              (ignore-errors (ai-send-command prompt-text))
              (claude-code-switch-to-buffer))))
      (message "Not in a git repository"))))

(defun ai-send-command (prompt-text)
  "Send PROMPT-TEXT to the AI service."
  (claude-code-send-command prompt-text))

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
(defun ai-prompt-code-change ()
  "Generate prompt to change code under cursor or in selected region.
If a region is selected, change that specific region.
Otherwise, change the function under cursor.
If nothing is selected and no function context, prompts for general code change.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive)
  (if (not buffer-file-name)
      (message "Error: buffer-file-name must be available")
    (let* ((function-name (which-function))
           (region-active (region-active-p)))
      (cond
       ;; 1) nothing selected
       ((not (or region-active function-name))
        (let* ((initial-prompt (aider-read-string "Change code: " ""))
               (final-prompt
                (concat initial-prompt
                        (format "\nFile: %s" buffer-file-name))))
          (ai-prompt--insert-prompt final-prompt)))
       ;; 2) region or function
       (region-active
        (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
               (prompt-label
                (if function-name
                    (format "Change code in function %s: " function-name)
                  "Change selected code: "))
               (initial-prompt (aider-read-string prompt-label))
               (final-prompt
                (concat initial-prompt
                        "\n\n" region-text
                        (when function-name
                          (format "\nFunction: %s" function-name))
                        (format "\nFile: %s" buffer-file-name))))
          (ai-prompt--insert-prompt final-prompt)))
       ;; 3) function
       (function-name
        (let* ((prompt-label (format "Change function %s: " function-name))
               (initial-prompt (aider-read-string prompt-label ""))
               (final-prompt
                (concat initial-prompt
                        (format "\nFunction: %s " function-name)
                        (format "\nFile: %s " buffer-file-name))))
          (ai-prompt--insert-prompt final-prompt)))))))

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
              (format "Please implement this code block:\n\n%s\n\nFile: %s"
                      region-text buffer-file-name))
             (is-comment
              (format "Please implement this comment:\n\n%s\n\nFile: %s"
                      current-line buffer-file-name))
             (function-name
              (format "Please implement TODO comments in function %s\n\nFile: %s"
                      function-name buffer-file-name))
             (t
              (format "Please implement TODO comments in file %s"
                      (file-name-nondirectory buffer-file-name)))))
           (prompt (aider-read-string "TODO implementation instruction: " initial-input)))
      (ai-prompt--insert-prompt prompt))))

;;;###autoload
(defun ai-prompt-ask-question ()
  "Generate prompt to ask questions about specific code.
If a region is selected, ask about that specific region.
If cursor is in a function, ask about that function.
Otherwise, ask a general question about the file.
Inserts the prompt into the AI prompt file and optionally sends to AI."
  (interactive)
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
                    (concat "\n\n" region-text))
                  (when function-name
                    (format "\nFunction: %s" function-name))
                  (when buffer-file-name
                    (format "\nFile: %s" buffer-file-name))
                  "\n\nNote: This is a question only - please do not modify the code.")))
    (ai-prompt--insert-prompt final-prompt)))

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
  ;; YASnippet support
  (when (require 'yasnippet nil t)
    (yas-minor-mode 1)
    (ai-prompt--setup-snippets)))

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(concat "/" (regexp-quote ai-prompt-file-name) "\'") . ai-prompt-mode))

;;;###autoload
(transient-define-prefix ai-prompt-menu ()
  "Transient menu for AI Prompt Mode interactive functions."
  ["AI Prompt Commands"
   ("!" "Start Claude Code" claude-code)
   ("z" "Switch to Claude Code" claude-code-switch-to-buffer)
   ("p" "Open prompt file" ai-prompt-open-prompt-file)
   ("c" "Code change" ai-prompt-code-change)
   ("q" "Ask question" ai-prompt-ask-question)
   ("t" "Implement TODO" ai-prompt-implement-todo)])

;;;###autoload
(global-set-key (kbd "C-c p") #'ai-prompt-menu)

(provide 'ai-prompt-mode)

;;; ai-prompt-mode.el ends here
