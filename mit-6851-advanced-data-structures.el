;;; mit-6851-advanced-data-structures.el --- Config for MIT 6.851 course -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration for studying MIT 6.851 Advanced Data Structures
;; Works both locally and through TRAMP to nexushive (nh)

;;; Code:

(require 'org)
(require 'ob)
(require 'tramp)

;;; Constants
(defconst mit-6851-nexushive-host "nh"
  "SSH alias for nexushive host.")

(defconst mit-6851-remote-base-dir "/ssh:nh:/home/jwalsh/ghq/github.com/aygp-dr/mit-6851-advanced-data-structures/"
  "Remote project directory on nexushive.")

(defconst mit-6851-local-base-dir "~/ghq/github.com/aygp-dr/mit-6851-advanced-data-structures/"
  "Local project directory.")

;;; Variables
(defvar mit-6851-current-location 'auto
  "Current location: 'local, 'remote, or 'auto.")

(defvar mit-6851-python-command "python3"
  "Python command to use.")

(defvar mit-6851-guile-command "guile3"
  "Guile command to use.")

;;; Location Detection
(defun mit-6851-on-nexushive-p ()
  "Check if we're running on nexushive."
  (string-match-p "nexushive\\|FreeBSD" (shell-command-to-string "uname -a")))

(defun mit-6851-get-project-root ()
  "Get the appropriate project root based on location."
  (cond
   ((eq mit-6851-current-location 'remote) mit-6851-remote-base-dir)
   ((eq mit-6851-current-location 'local) mit-6851-local-base-dir)
   ((mit-6851-on-nexushive-p) mit-6851-local-base-dir)
   (t mit-6851-remote-base-dir)))

;;; Org-mode Configuration
(defun mit-6851-setup-org-babel ()
  "Configure org-babel for MIT 6.851 course."
  ;; Load languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (scheme . t)
     (shell . t)
     (emacs-lisp . t)
     (ditaa . t)
     (dot . t)))
  
  ;; Set up Python
  (setq org-babel-python-command mit-6851-python-command)
  
  ;; Set up Scheme/Guile
  (setq geiser-active-implementations '(guile))
  (setq geiser-guile-binary mit-6851-guile-command)
  
  ;; Configure header arguments
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:session . "*MIT-6851-Python*")
          (:exports . "both")
          (:tangle . "yes")
          (:mkdirp . "yes")))
  
  (setq org-babel-default-header-args:scheme
        '((:results . "output")
          (:session . "*MIT-6851-Guile*")
          (:exports . "both")
          (:tangle . "yes")
          (:mkdirp . "yes")))
  
  ;; Mermaid support
  (setq org-babel-default-header-args:mermaid
        '((:results . "file")
          (:exports . "results")))
  
  ;; Don't ask for confirmation
  (setq org-confirm-babel-evaluate nil))

;;; TRAMP Configuration
(defun mit-6851-setup-tramp ()
  "Configure TRAMP for nexushive connection."
  ;; Use ssh control master for faster connections
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlMaster=auto "
         "-o ControlPath='~/.ssh/cm-%%r@%%h:%%p' "
         "-o ControlPersist=600"))
  
  ;; Set remote paths
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/usr/local/bin")
  
  ;; Python and Guile paths on FreeBSD
  (when (featurep 'tramp)
    (add-to-list 'tramp-remote-process-environment "PATH=/usr/local/bin:$PATH")))

;;; Project Navigation
(defun mit-6851-find-file ()
  "Find file in MIT 6.851 project."
  (interactive)
  (let ((default-directory (mit-6851-get-project-root)))
    (call-interactively 'find-file)))

(defun mit-6851-dired ()
  "Open dired in MIT 6.851 project root."
  (interactive)
  (dired (mit-6851-get-project-root)))

(defun mit-6851-open-session (session-name)
  "Open or create a session org file."
  (interactive "sSession name: ")
  (let* ((root (mit-6851-get-project-root))
         (session-file (concat root "sessions/" session-name ".org")))
    (find-file session-file)
    (when (= (buffer-size) 0)
      (mit-6851-insert-session-template session-name))))

(defun mit-6851-insert-session-template (session-name)
  "Insert a template for a new session."
  (insert (format "#+TITLE: MIT 6.851 - %s
#+AUTHOR: %s
#+DATE: %s
#+OPTIONS: toc:2 num:t ^:nil
#+PROPERTY: header-args :mkdirp yes
#+STARTUP: overview

* Setup
#+begin_src elisp :results silent
(load-file \"%smit-6851-advanced-data-structures.el\")
(mit-6851-setup-org-babel)
#+end_src

* Overview

* Implementation

** Python Implementation
#+begin_src python :tangle ../lib/%s.py
# %s implementation
#+end_src

** Scheme Implementation  
#+begin_src scheme :tangle ../lib/%s.scm
;;; %s implementation
#+end_src

* Diagrams

** Structure Diagram
#+begin_src mermaid :file ../docs/%s-structure.png
graph TD
    A[Start] --> B[Process]
    B --> C[End]
#+end_src

* Tests

#+begin_src python :tangle ../tests/test_%s.py
import unittest
from lib.%s import *

class Test%s(unittest.TestCase):
    def test_basic(self):
        pass

if __name__ == '__main__':
    unittest.main()
#+end_src

* Notes

" 
                  session-name
                  user-full-name
                  (format-time-string "%Y-%m-%d")
                  (if (string-prefix-p "/ssh:" default-directory) 
                      (concat (file-remote-p default-directory) mit-6851-local-base-dir)
                    "")
                  (downcase (replace-regexp-in-string " " "_" session-name))
                  session-name
                  (downcase (replace-regexp-in-string " " "_" session-name))
                  session-name
                  (downcase (replace-regexp-in-string " " "_" session-name))
                  (downcase (replace-regexp-in-string " " "_" session-name))
                  (downcase (replace-regexp-in-string " " "_" session-name))
                  (capitalize (replace-regexp-in-string "[- ]" "" session-name)))))

;;; Remote Execution Helpers
(defun mit-6851-run-python-remote (code)
  "Run Python code on remote host."
  (let ((default-directory mit-6851-remote-base-dir))
    (shell-command-to-string 
     (format "python3 -c %s" (shell-quote-argument code)))))

(defun mit-6851-run-guile-remote (code)
  "Run Guile code on remote host."
  (let ((default-directory mit-6851-remote-base-dir))
    (shell-command-to-string 
     (format "guile3 -c %s" (shell-quote-argument code)))))

;;; Interactive Commands
(defun mit-6851-switch-location ()
  "Switch between local and remote locations."
  (interactive)
  (setq mit-6851-current-location
        (if (eq mit-6851-current-location 'remote) 'local 'remote))
  (message "Switched to %s location" mit-6851-current-location))

(defun mit-6851-run-tests ()
  "Run tests for current session."
  (interactive)
  (let ((default-directory (concat (mit-6851-get-project-root) "tests/")))
    (compile "python3 -m pytest -v")))

(defun mit-6851-tangle-and-run ()
  "Tangle current org file and run the resulting code."
  (interactive)
  (org-babel-tangle)
  (let* ((file-name (file-name-base (buffer-file-name)))
         (py-file (format "%slib/%s.py" (mit-6851-get-project-root) file-name))
         (scm-file (format "%slib/%s.scm" (mit-6851-get-project-root) file-name)))
    (when (file-exists-p py-file)
      (compile (format "cd %s && python3 %s" 
                       (mit-6851-get-project-root) 
                       (file-relative-name py-file (mit-6851-get-project-root)))))
    (when (file-exists-p scm-file)
      (compile (format "cd %s && guile3 %s"
                       (mit-6851-get-project-root)
                       (file-relative-name scm-file (mit-6851-get-project-root)))))))

;;; Key Bindings
(defvar mit-6851-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m f") 'mit-6851-find-file)
    (define-key map (kbd "C-c m d") 'mit-6851-dired)
    (define-key map (kbd "C-c m s") 'mit-6851-open-session)
    (define-key map (kbd "C-c m l") 'mit-6851-switch-location)
    (define-key map (kbd "C-c m t") 'mit-6851-run-tests)
    (define-key map (kbd "C-c m r") 'mit-6851-tangle-and-run)
    map)
  "Keymap for MIT 6.851 commands.")

;;; Minor Mode
(define-minor-mode mit-6851-mode
  "Minor mode for MIT 6.851 Advanced Data Structures course."
  :lighter " MIT6851"
  :keymap mit-6851-mode-map
  (when mit-6851-mode
    (mit-6851-setup-org-babel)
    (mit-6851-setup-tramp)))

;;; Auto-activation
(defun mit-6851-maybe-enable ()
  "Enable MIT 6.851 mode if in project directory."
  (when (and buffer-file-name
             (or (string-match-p "mit-6851-advanced-data-structures" buffer-file-name)
                 (string-match-p "MIT.*6851" buffer-file-name)))
    (mit-6851-mode 1)))

(add-hook 'org-mode-hook 'mit-6851-maybe-enable)
(add-hook 'python-mode-hook 'mit-6851-maybe-enable)
(add-hook 'scheme-mode-hook 'mit-6851-maybe-enable)

;;; Main Setup Function
(defun mit-6851-setup ()
  "Set up MIT 6.851 environment."
  (interactive)
  (mit-6851-setup-org-babel)
  (mit-6851-setup-tramp)
  (message "MIT 6.851 environment configured for %s" 
           (if (mit-6851-on-nexushive-p) "local nexushive" "remote via TRAMP")))

(provide 'mit-6851-advanced-data-structures)
;;; mit-6851-advanced-data-structures.el ends here
