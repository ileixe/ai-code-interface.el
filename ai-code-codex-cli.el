;;; ai-code-codex-cli.el --- Thin wrapper for Codex CLI  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Thin wrapper that reuses `claude-code' to run Codex CLI.
;; Provides interactive commands and aliases for the AI Code suite.
;;
;;; Code:

(require 'claude-code)

(defgroup ai-code-codex-cli nil
  "Codex CLI integration via `claude-code'."
  :group 'tools
  :prefix "codex-cli-")

(defcustom codex-cli-program "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'ai-code-codex-cli)

;;;###autoload
(defun codex-cli (&optional arg)
  "Start Codex (reuses `claude-code' startup logic)."
  (interactive "P")
  (let ((claude-code-program codex-cli-program) ; override dynamically
        (claude-code-program-switches nil))         ; optional e.g.: '("exec" "--non-interactive")
    (claude-code arg)))

;;;###autoload
(defun codex-cli-switch-to-buffer ()
  (interactive)
  (claude-code-switch-to-buffer))

;;;###autoload
(defun codex-cli-send-command (line)
  (interactive "sCodex> ")
  (claude-code-send-command line))


(provide 'ai-code-codex-cli)

;;; ai-code-codex-cli.el ends here
