;;; ai-code-prompt-mode.el --- AI code prompt mode for editing AI prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides a major mode for editing AI prompt files.

;;; Code:

(require 'org)
(require 'magit)

(defvar yas-snippet-dirs)

(declare-function magit-toplevel "magit" (&optional dir))
(declare-function gptel-get-answer "gptel-assistant" (prompt))

(defvar ai-code-auto-send-to-ai)
(defvar ai-code-use-gptel-headline)
(defvar ai-code-prompt-suffix)

(declare-function yas-load-directory "yasnippet" (dir))
(declare-function yas-minor-mode "yasnippet")
(declare-function ai-code-cli-send-command "ai-code-interface" (command))
(declare-function ai-code-cli-switch-to-buffer "ai-code-interface" ())

;;;###autoload
(defcustom ai-code-prompt-file-name ".ai.code.prompt.org"
  "File name that will automatically enable `ai-code-prompt-mode` when opened.
This is the file name without path."
  :type 'string
  :group 'ai-code)

(defun ai-code--setup-snippets ()
  "Setup YASnippet directories for `ai-code-prompt-mode`."
  (condition-case nil
      (when (require 'yasnippet nil t)
        (let ((snippet-dir (expand-file-name "snippets"
                                             (file-name-directory (file-truename (locate-library "ai-code-interface"))))))
          (when (file-directory-p snippet-dir)
            (unless (boundp 'yas-snippet-dirs)
              (setq yas-snippet-dirs nil))
            (add-to-list 'yas-snippet-dirs snippet-dir t)
            (ignore-errors (yas-load-directory snippet-dir))))
    (error nil)))) ;; Suppress all errors

;;;###autoload
(defun ai-code-open-prompt-file ()
  "Open AI prompt file under git repo root.
If file doesn't exist, create it with sample prompt."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (prompt-file (when git-root
                        (expand-file-name ai-code-prompt-file-name git-root))))
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

(defun ai-code--get-ai-code-prompt-file-path ()
  "Get the path to the AI prompt file in the current git repository."
  (let* ((git-root (magit-toplevel)))
    (when git-root
      (expand-file-name ai-code-prompt-file-name git-root))))

(defun ai-code--execute-command (command)
  "Execute COMMAND directly without saving to prompt file."
  (message "Executing command: %s" command)
  (ignore-errors (ai-code-cli-send-command command))
  (ai-code-cli-switch-to-buffer))

(defun ai-code--generate-prompt-headline (prompt-text)
  "Generate and insert a headline for PROMPT-TEXT."
  (insert "** ")
  (if ai-code-use-gptel-headline
      (condition-case nil
          (let ((headline (gptel-get-answer (concat "Create a 5-10 word action-oriented headline for this AI prompt that captures the main task. Use keywords like: refactor, implement, fix, optimize, analyze, document, test, review, enhance, add, remove, improve, integrate, task. Example: 'Optimize database queries' or 'Implement error handling'.\n\nPrompt: " prompt-text))))
            (insert headline " ")
            (org-insert-time-stamp (current-time) t t))
        (error (org-insert-time-stamp (current-time) t t)))
    (org-insert-time-stamp (current-time) t t))
  (insert "\n"))

(defun ai-code--format-and-insert-prompt (prompt-text)
  "Format PROMPT-TEXT with suffix and insert into the current buffer."
  (let ((full-prompt (if ai-code-prompt-suffix
                         (concat prompt-text "\n" ai-code-prompt-suffix "\n")
                       prompt-text)))
    (insert full-prompt)
    (unless (bolp)
      (insert "\n"))
    full-prompt))

(defun ai-code--get-prompt-buffer (prompt-file)
  "Get the buffer for PROMPT-FILE.
If `ai-code-auto-send-to-ai` is non-nil, open file without selecting,
otherwise open in another window."
  (if ai-code-auto-send-to-ai
      (find-file-noselect prompt-file)
    (find-file-other-window prompt-file)))

(defun ai-code--append-prompt-to-buffer (prompt-text)
  "Append formatted PROMPT-TEXT to the end of the current buffer.
This includes generating a headline and formatting the prompt.
Returns the full prompt text that was inserted."
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n\n"))
  (ai-code--generate-prompt-headline prompt-text)
  (ai-code--format-and-insert-prompt prompt-text))

(defun ai-code--auto-send-prompt (full-prompt)
  "If `ai-code-auto-send-to-ai` is set, send FULL-PROMPT to AI."
  (when ai-code-auto-send-to-ai
    (ignore-errors (ai-code-cli-send-command full-prompt))
    (ai-code-cli-switch-to-buffer)))

(defun ai-code--write-prompt-to-file (prompt-text)
  "Write PROMPT-TEXT to the AI prompt file."
  (let ((prompt-file (ai-code--get-ai-code-prompt-file-path)))
    (when prompt-file
      (let ((buffer (ai-code--get-prompt-buffer prompt-file)))
        (with-current-buffer buffer
          (let ((full-prompt (ai-code--append-prompt-to-buffer prompt-text)))
            (save-buffer)
            (message "Prompt added to %s" prompt-file)
            (ai-code--auto-send-prompt full-prompt)))))))

(defun ai-code--preprocess-prompt-text (prompt-text)
  "Preprocess PROMPT-TEXT to replace file paths with relative paths prefixed with @.
The function splits the prompt by whitespace, checks if each part is a file
path within the current git repository, and if so, replaces it.
NOTE: This does not handle file paths containing spaces."
  (if-let ((git-root (magit-toplevel)))
      (mapconcat
       (lambda (word)
         (let ((expanded-word (expand-file-name word)))
           (if (and (file-exists-p expanded-word)
                    (string-prefix-p git-root (file-truename expanded-word)))
               (concat "@" (file-relative-name expanded-word git-root))
             word)))
       (split-string prompt-text "[ \t\n]+" t) ; split by whitespace and remove empty strings
       " ")
    ;; Not in a git repo, return original prompt
    prompt-text))

(defun ai-code--insert-prompt (prompt-text)
  "Preprocess and insert PROMPT-TEXT into the AI prompt file, or execute if it's a command."
  (let ((processed-prompt (ai-code--preprocess-prompt-text prompt-text)))
    (if (and (string-prefix-p "/" processed-prompt)
             (not (string-match-p " " processed-prompt)))
        (ai-code--execute-command processed-prompt)
      (ai-code--write-prompt-to-file processed-prompt))))

;; Define the AI Prompt Mode (derived from org-mode)
;;;###autoload
(define-derived-mode ai-code-prompt-mode org-mode "AI Prompt"
  "Major mode derived from `org-mode` for editing AI prompt files.
Special commands:
\{ai-code-prompt-mode-map}"
  ;; Basic setup
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local truncate-lines nil)  ; Disable line truncation, allowing lines to wrap
  (define-key ai-code-prompt-mode-map (kbd "C-c C-c") #'ai-code-prompt-send-block)
  ;; YASnippet support
  (when (require 'yasnippet nil t)
    (yas-minor-mode 1)
    (ai-code--setup-snippets)))

;;;###autoload
(defun ai-code-prompt-send-block ()
  "Send the current text block (paragraph) to the AI service.
The block is the text separated by blank lines. It trims leading/trailing whitespace."
  (interactive)
  (let* ((block-text (thing-at-point 'paragraph))
         (trimmed-text (when block-text (string-trim block-text))))
    (if (and trimmed-text (string-match-p "\\S-" trimmed-text))
        (progn
          (ai-code-cli-send-command trimmed-text)
          (ai-code-cli-switch-to-buffer))
      (message "No text in the current block to send."))))

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(concat "/" (regexp-quote ai-code-prompt-file-name) "\'") . ai-code-prompt-mode))

(provide 'ai-code-prompt-mode)

;;; ai-code-prompt-mode.el ends here
