;;; ai-code-input.el --- Helm completion for ai-code-interface.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/ai-code-interface.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional Helm completion interface for ai-code-interface.el
;; To use this, ensure both ai-code-interface.el and helm are installed.

;;; Code:

(require 'cl-lib)  ; For `cl-subseq`

(declare-function helm-comp-read "helm-mode" (prompt collection &rest args))

;;;###autoload
(defun ai-code-plain-read-string (prompt &optional initial-input candidate-list)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
CANDIDATE-LIST provides additional completion options if provided.
This function combines candidate-list with history for better completion."
  ;; Combine candidate-list with history, removing duplicates
  (let ((completion-candidates
         (delete-dups (append candidate-list
                              (when (boundp 'ai-code-read-string-history)
                                ai-code-read-string-history)))))
    ;; Use completing-read with the combined candidates
    (completing-read prompt
                     completion-candidates
                     nil nil initial-input
                     'ai-code-read-string-history)))

;;;###autoload
(defalias 'ai-code-read-string #'ai-code-plain-read-string)

(defun ai-code--get-git-repo-root ()
  "Return the top-level directory of the current git repository, or nil."
  (let ((git-root (magit-toplevel)))
    (when (and git-root (stringp git-root) (not (string-match-p "fatal" git-root)))
      (file-truename git-root))))

(defun ai-code--get-relevant-directory-for-history ()
  "Return the top-level directory of the current git repository.
If not in a git repo, return the directory of the current buffer's file.
Returns nil if neither can be determined."
  (or (ai-code--get-git-repo-root)
      (when-let ((bfn (buffer-file-name)))
        (file-name-directory (file-truename bfn)))))

(defun ai-code--generate-history-file-name ()
  "Generate path for .aider.input.history in git repo root or current buffer's dir."
  (when-let ((relevant-dir (ai-code--get-relevant-directory-for-history)))
    (expand-file-name ".aider.input.history" relevant-dir)))

;;; History parsing

(defun ai-code--parse-ai-code-cli-history (file-path)
  "Parse .aider.input.history file at FILE-PATH.
Return a list of commands, oldest to newest."
  (when (and file-path (file-readable-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((history-items '())
            (current-multi-line-command-parts nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (when (string-match "^\\+[ \t]*\\(.*\\)" line)
              (let ((content (match-string 1 line)))
                (cond
                 ((string= content "{aider")
                  (setq current-multi-line-command-parts (list content)))
                 ((string= content "aider}")
                  (if current-multi-line-command-parts
                      (progn
                        (setq current-multi-line-command-parts
                              (nconc current-multi-line-command-parts (list content)))
                        (push (string-join current-multi-line-command-parts "\n")
                              history-items)
                        (setq current-multi-line-command-parts nil))
                    (push content history-items)))
                 (current-multi-line-command-parts
                  (setq current-multi-line-command-parts
                        (nconc current-multi-line-command-parts (list content))))
                 (t
                  (push content history-items))))))
          (forward-line 1))
        (when current-multi-line-command-parts
          (push (string-join current-multi-line-command-parts "\n")
                history-items))
        (reverse history-items)))))

(defun ai-code-helm-read-string-with-history (prompt history-file-name &optional initial-input candidate-list)
  "Read a string with Helm completion using specified history file.
PROMPT is the prompt string.
HISTORY-FILE-NAME is the base name for history file.
INITIAL-INPUT is optional initial input string.
CANDIDATE-LIST is an optional list of candidate strings to show before history."
  ;; Load history from file
  (let* ((helm-history-file (expand-file-name history-file-name user-emacs-directory))
         (helm-history (if (file-exists-p helm-history-file)
                           (with-temp-buffer
                             (insert-file-contents helm-history-file)
                             (read (buffer-string))) ; Assumed newest first
                         '()))
         (cli-history-file-path (ai-code--generate-history-file-name))
         (parsed-cli-history (if cli-history-file-path
                                 (ai-code--parse-ai-code-cli-history cli-history-file-path) ; Oldest first
                               '()))
         (cli-history-newest-first (reverse parsed-cli-history))
         (cli-history-newest-first
          (cl-remove-if-not (lambda (s) (not (string-match "\n" s)))
                            cli-history-newest-first))
         ;; Combine Helm history and CLI history, then deduplicate. Helm history items take precedence.
         (history (delete-dups (append helm-history cli-history-newest-first)))
         ;; Extract the most recent item from history (if exists)
         (most-recent (when (and history (not (null history)))
                        (car history)))
         ;; Remove the first item to add it back later
         (rest-history (when (and history (not (null history)))
                         (cdr history)))
         ;; Combine completion list: most recent + candidates + separator + rest of history
         (completion-list
          (append
           ;; If most recent item exists, put it at the top
           (when most-recent
             (list most-recent))
           ;; Add candidate list
           (or candidate-list '())
           ;; Add separator and rest of history
           (when rest-history
             (cons "==================== HISTORY ========================================" rest-history))))
         ;; Read input with helm
         (input (helm-comp-read
                 prompt
                 completion-list
                 :must-match nil
                 :name "Helm Read String, Use C-c C-y to edit selected command. C-b and C-f to move cursor during editing"
                 :fuzzy t
                 :initial-input initial-input)))
    ;; Add to history if non-empty, single-line and save
    (unless (or (string-empty-p input) (string-match "\n" input))
      (push input history)
      ;; (setq history (mapcar #'substring-no-properties history))
      (with-temp-file helm-history-file ; Save to the Helm-specific history file
        (let ((history-entries (cl-subseq history
                                          0 (min (length history)
                                                 10000))))  ; Keep last 10000 entries
          (insert (let ((print-circle nil))
                    (prin1-to-string history-entries))))))
    input))

(defun ai-code-helm-read-string (prompt &optional initial-input candidate-list)
  "Read a string with Helm completion for ai-code, showing historical inputs.
PROMPT is the prompt string.
INITIAL-INPUT is optional initial input string.
CANDIDATE-LIST is an optional list of candidate strings to show before history."
  (ai-code-helm-read-string-with-history prompt "ai-code-helm-read-string-history.el" initial-input candidate-list))

;;;###autoload
(if (featurep 'helm)
    (defalias 'ai-code-read-string #'ai-code-helm-read-string))

(provide 'ai-code-input)
;;; ai-code-input.el ends here
