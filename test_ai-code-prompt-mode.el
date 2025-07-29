
(require 'ert)
(require 'ai-code-prompt-mode)
(require 'magit)
(require 'cl-lib)

;; Helper macro to set up and tear down the test environment
(defmacro with-test-repo (&rest body)
  "Set up a temporary git repository environment for testing.
This macro creates a temporary directory structure, mocks `magit-toplevel`,
and ensures everything is cleaned up afterward."
  `(let* ((git-root (expand-file-name "test-repo/" temporary-file-directory))
          (mock-file-in-repo (expand-file-name "src/main.js" git-root))
          (outside-file (expand-file-name "other-file.txt" temporary-file-directory)))
     (unwind-protect
         (progn
           ;; Setup: Create dummy files and directories
           (make-directory (file-name-directory mock-file-in-repo) t)
           (with-temp-file mock-file-in-repo (insert "content"))
           (with-temp-file outside-file (insert "content"))
           ;; Execute test body with mocks
           (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) git-root)))
             ,@body))
       ;; Teardown: Clean up dummy files and directories
       (when (file-exists-p mock-file-in-repo) (delete-file mock-file-in-repo))
       (when (file-exists-p outside-file) (delete-file outside-file))
       (when (file-directory-p (file-name-directory mock-file-in-repo))
         (delete-directory (file-name-directory mock-file-in-repo)))
       (when (file-directory-p git-root) (delete-directory git-root)))))

(ert-deftest test-preprocess-path-in-repo ()
  "Test that a file path inside the git repo is made relative with an @-prefix."
  (with-test-repo
   (let ((prompt (format "check file %s" mock-file-in-repo)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      "check file @src/main.js")))))

(ert-deftest test-preprocess-path-outside-repo ()
  "Test that a file path outside the git repo remains unchanged."
  (with-test-repo
   (let ((prompt (format "check file %s" outside-file)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest test-preprocess-non-existent-path ()
  "Test that a non-existent file path remains unchanged."
  (with-test-repo
   (let ((prompt "check file /tmp/non-existent-file.txt"))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest test-preprocess-prompt-without-path ()
  "Test that a prompt with no file paths remains unchanged."
  (with-test-repo
   (let ((prompt "this is a simple prompt"))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      prompt)))))

(ert-deftest test-preprocess-multiple-paths ()
  "Test a prompt with multiple file paths (inside and outside the repo)."
  (with-test-repo
   (let ((prompt (format "compare %s and %s" mock-file-in-repo outside-file)))
     (should (string= (ai-code--preprocess-prompt-text prompt)
                      (format "compare @src/main.js and %s" outside-file))))))

(ert-deftest test-preprocess-not-in-git-repo ()
  "Test that paths are not modified when not in a git repository."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda (&optional dir) nil)))
    (let ((prompt "check file /some/file.txt"))
      (should (string= (ai-code--preprocess-prompt-text prompt)
                       prompt)))))
