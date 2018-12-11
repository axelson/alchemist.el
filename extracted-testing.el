;; Variables
(defvar alchemist-last-run-test nil)

(defvar alchemist-project-root-path-cache nil
  "Variable which holds the cached project root path.")

(defcustom alchemist-mix-command "mix"
  "The shell command for mix."
  :type 'string
  :group 'alchemist-mix)

(defcustom alchemist-mix-test-task "test"
  "Default task to run tests."
  :type 'string
  :group 'alchemist-mix)

(defcustom alchemist-mix-test-default-options '()
  "Default options for alchemist test command."
  :type '(repeat string)
  :group 'alchemist-mix)

;; Test mode-variables
(defconst alchemist-test-report-process-name "alchemist-test-process"
  "Name of the test report process.")

(defconst alchemist-test-report-buffer-name "*alchemist test report*"
  "Name of the test report buffer.")


;; Code

(defun alchemist-mix-test-at-point ()
  "Run the test at point."
  (interactive)
  (let* ((line (line-number-at-pos (point)))
         (file-and-line (format "%s:%s" buffer-file-name line)))
    (alchemist-mix--execute-test file-and-line)))

(defun alchemist-mix-rerun-last-test ()
  "Rerun the last test that was run by alchemist.

When no tests had been run before calling this function, do nothing."
  (interactive)
  (if alchemist-last-run-test
      (alchemist-mix--execute-test alchemist-last-run-test)
    (message "No tests have been run yet")))

(defun alchemist-mix--execute-test (&optional what)
  "Execute 'mix test' on the given `WHAT'.

`WHAT' could be a filename, a filename:line string or the empty string (meaning
run all tests)."
  (if what
      (setq alchemist-last-run-test what)
    (setq alchemist-last-run-test ""))
  (alchemist-test-execute (list alchemist-mix-command
                                alchemist-mix-test-task
                                what
                                alchemist-mix-test-default-options)))

(defun alchemist-mix--test-file (filename)
  "Run a specific FILENAME as argument for the mix command test."
  (when (not (file-exists-p filename))
    (error "The given file doesn't exist"))
  (alchemist-mix--execute-test (expand-file-name filename)))

(defun alchemist-test-execute (command-list)
  (message "Testing...")
  (let* ((command (mapconcat 'concat (-flatten command-list) " ")))
    (alchemist-test-save-buffers)
    (alchemist-report-run command
                          alchemist-test-report-process-name
                          alchemist-test-report-buffer-name
                          'alchemist-test-report-mode
                          #'alchemist-test--handle-exit)))

(defun alchemist-test-save-buffers ()
  "Save some modified file-visiting buffers."
  (save-some-buffers (not alchemist-test-ask-about-save) nil))


(defun alchemist-report-run (command process-name buffer-name mode &optional on-exit hidden)
  "Run COMMAND in a new process called PROCESS-NAME.
The output of PROCESS-NAME will be displayed in BUFFER-NAME.
After displaying BUFFER-NAME, the MODE function will be called within.

Optional ON-EXIT and HIDDEN functions could be defined.
The function ON-EXIT will be called when PROCESS-NAME is finished.
The HIDDEN variable defines if PROCESS-NAME should run in the background."
  (let* ((buffer (get-buffer-create buffer-name))
         (default-directory (alchemist-project-root-or-default-dir)))
    (alchemist-report-cleanup-process-buffer buffer)
    (alchemist-report--kill-process (get-buffer-process buffer))
    (start-process-shell-command process-name buffer command)
    (when on-exit
      (setq alchemist-report-on-exit-function on-exit))
    (set-process-sentinel (get-buffer-process buffer) 'alchemist-report--sentinel)
    (set-process-filter (get-buffer-process buffer) 'alchemist-report-filter)
    (alchemist-report-activate-mode mode buffer)
    (if (not hidden)
        (alchemist-report-display-buffer buffer))
    (alchemist-report-update-mode-name (get-buffer-process buffer))))

(defun alchemist-project-root-or-default-dir ()
  "Return the current Elixir mix project root or `default-directory'."
  (let* ((project-root (alchemist-project-root))
         (dir (if project-root
                  project-root
                default-directory)))
    dir))

(defun alchemist-project-root (&optional dir)
  "Return root directory of the current Elixir Mix project.

It starts walking the directory tree to find the Elixir Mix root directory
from `default-directory'. If DIR is non-nil it starts walking the
directory from there instead."
  (if (and alchemist-project-root-path-cache
	   (string-prefix-p alchemist-project-root-path-cache
			    (expand-file-name default-directory)))
      alchemist-project-root-path-cache
    (let* ((dir (file-name-as-directory (or dir (expand-file-name default-directory))))
	   (present-files (directory-files dir)))
      (cond ((alchemist-project-top-level-dir-p dir)
	     nil)
	    ((-contains-p present-files alchemist-project-hex-pkg-indicator)
	     (alchemist-project-root (file-name-directory (directory-file-name dir))))
	    ((-contains-p present-files alchemist-project-mix-project-indicator)
	     (setq alchemist-project-root-path-cache dir)
	     dir)
	    (t
	     (alchemist-project-root (file-name-directory (directory-file-name dir))))))))

(defvar alchemist-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c , s") alchemist-test-at-point)
    (define-key map (kbd "C-c , v") alchemist-test-this-buffer)
    (define-key map (kbd "C-c , a") alchemist-test)
    (define-key map (kbd "C-c , f") alchemist-test-file)
    (define-key map (kbd "C-c , p") alchemist-test-jump-to-previous-test)
    (define-key map (kbd "C-c , n") alchemist-test-jump-to-next-test)
    (define-key map (kbd "C-c , l") alchemist-test-list-tests)
    map)
  "Keymap for `alchemist-test-mode'.")

;; Need to still pull these in
;;;; Variables
;; 'alchemist-test-report-mode
;; #'alchemist-test--handle-exit)))


;; Notes
;; Might need to pull in alchemist-compile.el to compile files before running the tests
;; Also look at alchemist-project
