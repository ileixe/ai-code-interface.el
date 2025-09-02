;; codex-cli.el — Thin wrapper; do not modify claude-code.el
(require 'claude-code)

(defcustom codex-cli-program "codex"
  "Path to the Codex CLI executable."
  :type 'string)

(defun codex-cli (&optional arg)
  "Start Codex (reuses `claude-code' startup logic)."
  (interactive "P")
  (let ((claude-code-program codex-cli-program) ; override dynamically
        (claude-code-default-args nil))         ; optional e.g.: '("exec" "--non-interactive")
    (claude-code arg)))

(defun codex-cli-switch-to-buffer ()
  (interactive)
  (claude-code-switch-to-buffer))

(defun codex-cli-send-command (line)
  (interactive "sCodex> ")
  (claude-code-send-command line))

(defalias 'ai-code-cli-start #'codex-cli)
(defalias 'ai-code-cli-switch-to-buffer #'codex-cli-switch-to-buffer)
(defalias 'ai-code-cli-send-command #'codex-cli-send-command)
