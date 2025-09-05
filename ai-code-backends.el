;;; ai-code-backends.el --- Backend selection support for ai-code -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Backend selection support extracted from ai-code-interface.el.

;;; Code:

(require 'seq)

(defvar ai-code-cli)

;;;###autoload
(defcustom ai-code-backends
  '((claude-code
     :label "claude-code.el"
     :require claude-code
     :start   claude-code
     :switch  claude-code-switch-to-buffer
     :send    claude-code-send-command
     :cli     "claude")
    (claude-code-ide
     :label "claude-code-ide.el"
     :require claude-code-ide
     :start   claude-code-ide--start-if-no-session
     :switch  claude-code-ide-switch-to-buffer
     :send    claude-code-ide-send-prompt
     :cli     "claude")
    (gemini
      :label "gemini-cli.el"
      :require gemini-cli
      :start   gemini-cli
      :switch  gemini-cli-switch-to-buffer
      :send    gemini-cli-send-command
      :cli     "gemini")
    (codex
     :label "ai-code-codex-cli.el"
     :require ai-code-codex-cli
     :start   codex-cli
     :switch  codex-cli-switch-to-buffer
     :send    codex-cli-send-command
     :cli     "codex"))
  "Available AI backends and how to integrate with them.
Each entry is (KEY :label STRING :require FEATURE :start FN :switch FN :send FN :cli STRING)."
  :type '(repeat (list (symbol :tag "Key")
                       (const :label) (string :tag "Label")
                       (const :require) (symbol :tag "Feature to require")
                       (const :start) (symbol :tag "Start function")
                       (const :switch) (symbol :tag "Switch function")
                       (const :send) (symbol :tag "Send function")
                       (const :cli) (string :tag "CLI name")))
  :group 'ai-code)

(defvar ai-code-selected-backend 'claude-code
  "Currently selected backend key from `ai-code-backends'.")

(defun ai-code--backend-spec (key)
  "Return backend plist for KEY from `ai-code-backends'."
  (seq-find (lambda (it) (eq (car it) key)) ai-code-backends))

(defun ai-code-current-backend-label ()
  "Return label string of the currently selected backend.
Falls back to symbol name when label is unavailable."
  (let* ((spec (ai-code--backend-spec ai-code-selected-backend))
         (label (when spec (plist-get (cdr spec) :label))))
    (or label (and ai-code-selected-backend (symbol-name ai-code-selected-backend)) "<none>")))

(defun ai-code--ensure-backend-loaded (spec)
  "Ensure FEATURE for backend SPEC is loaded, if any."
  (let* ((plist (cdr spec))
         (feature (plist-get plist :require)))
    (when feature (require feature nil t))))

(defun ai-code--apply-backend (key)
  "Apply backend identified by KEY.
Sets `ai-code-cli-*' defaliases and updates `ai-code-cli'."
  (let* ((spec (ai-code--backend-spec key)))
    (unless spec
      (user-error "Unknown backend: %s" key))
    (ai-code--ensure-backend-loaded spec)
    (let* ((plist (cdr spec))
           (label  (plist-get plist :label))
           (feature (plist-get plist :require))
           (start  (plist-get plist :start))
           (switch (plist-get plist :switch))
           (send   (plist-get plist :send))
           (cli    (plist-get plist :cli)))
      ;; If the declared feature is not available after require, inform user to install it.
      (when (and feature (not (featurep feature)))
        (user-error "Backend '%s' is not available. Please install the package providing '%s' and try again."
                    label (symbol-name feature)))
      (unless (and (fboundp start) (fboundp switch) (fboundp send))
        (user-error "Backend '%s' is not available (missing functions). Please install the package providing '%s'."
                    label (symbol-name feature)))
      (defalias 'ai-code-cli-start start)
      (defalias 'ai-code-cli-switch-to-buffer switch)
      (defalias 'ai-code-cli-send-command send)
      (setq ai-code-cli cli
            ai-code-selected-backend key)
      (message "AI Code backend switched to: %s" (plist-get plist :label)))))

;;;###autoload
(defun ai-code-select-backend ()
  "Interactively select and apply an AI backend from `ai-code-backends'."
  (interactive)
  (let* ((choices (mapcar (lambda (it)
                            (let* ((key (car it))
                                   (label (plist-get (cdr it) :label)))
                              (cons (format "%s" label) key)))
                          ai-code-backends))
         (choice (completing-read "Select backend: " (mapcar #'car choices) nil t))
         (key (cdr (assoc choice choices))))
    (ai-code--apply-backend key)))

(provide 'ai-code-backends)

;;; ai-code-backends.el ends here
