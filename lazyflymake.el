;;; lazyflymake.el --- Lightweight syntax checker for Emacs, alternative of `flymake-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2020 Chen Bin
;;
;; Version: 0.0.1
;; Keywords: convenience, languages, tools
;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
;; URL: https://github.com/redguardtoo/lazyflymake
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;  Remove "(flymake-mode 1)" from ~/.emacs and insert below line instead,
;;
;;    (add-hook 'prog-mode-hook #'lazyflymake-start)
;;
;;  Done !
;;
;; Please note this program also set up flymake for Shell script, Emacs Lisp,
;; and Lua automatically.
;;
;; Shellcheck (https://github.com/koalaman/shellcheck) is required to check
;; shell script.
;;
;; Lua executable is required to check Lua code.
;;

;;; Code:
(require 'cl-lib)
(require 'lazyflymake-sdk)

(defcustom lazyflymake-update-interval 2
  "Interval (seconds) for `lazyflymake-check-buffer'."
  :group 'lazyflymake
  :type 'integer)

(defcustom lazyflymake-shell-script-modes '(sh-mode)
  "Major modes for shell script."
  :group 'lazyflymake
  :type '(repeat 'sexp))

(defcustom lazyflymake-check-buffer-max (* 128 1024 1024)
  "Max size of buffer to run `lazyflymake-check-buffer'."
  :type 'integer
  :group 'lazyflymake)

;; Timer to run auto-update tags file
(defvar lazyflymake-timer nil "Internal timer.")

(defvar lazyflymake-start-check-now nil
  "If it's t, `lazyflymake-start' starts buffer syntax check immediately.
This variable is for debug and unit test only.")

(defun lazyflymake-load(file-name-regexp mask)
  "Load flymake MASK for files matching FILE-NAME-REGEXP."
  (let* ((lib (intern (concat "lazyflymake-" (symbol-name mask))))
         (prefix (concat "lazyflymake-" (symbol-name mask)))
         (init-fn (intern (format "%s-%s" prefix "init")))
         (pattern-fn (intern (format "%s-%s" prefix "err-line-pattern"))))

    (when lazyflymake-debug
      (message "lazyflymake-load: mask=%s regexp=%s code-file=%s"
               mask
               file-name-regexp
               buffer-file-name))

    ;; load the library
    (when (and buffer-file-name
               (string-match file-name-regexp buffer-file-name)
               ;; respect existing set up in `flymake-allowed-file-name-masks'
               (not (cl-find-if `(lambda (e) (string= (car e) ,file-name-regexp))
                                flymake-allowed-file-name-masks)))
      (unless (featurep lib) (require lib))
      (let* ((pattern (funcall pattern-fn)))
        (if lazyflymake-debug (message "pattern=%s" pattern))
        (when pattern
          (cond
           ((stringp (car pattern))
            (push pattern flymake-err-line-patterns))
           ((listp pattern)
            (setq-local flymake-err-line-patterns pattern)))))
      ;; in-case someone empty this variable
      (unless flymake-diagnostic-functions
        (setq flymake-diagnostic-functions '(flymake-start-syntax-check)))
      (push (list file-name-regexp init-fn) flymake-allowed-file-name-masks))))

(defun lazyflymake-start-now ()
  "Check current buffer right now."
  ;; `flymake-start' need this hash table
  (unless flymake--backend-state
    (setq flymake--backend-state (make-hash-table)))
  ;; start checking immediately
  (flymake-start nil t))

(defun lazyflymake-check-buffer ()
  "Spell check current buffer."
  (if lazyflymake-debug (message "lazyflymake-check-buffer called."))
  (cond
   ((not lazyflymake-timer)
    ;; start timer if not started yet
    (setq lazyflymake-timer (current-time)))

   ((< (- (float-time (current-time)) (float-time lazyflymake-timer))
       lazyflymake-update-interval)
    ;; do nothing, avoid `flyspell-buffer' too often
    (if lazyflymake-debug "Flymake syntax check skipped."))

   (t
    ;; check
    (setq lazyflymake-timer (current-time))
    (when (and (< (buffer-size) lazyflymake-check-buffer-max))
      (lazyflymake-start-now)
      (if lazyflymake-debug (message "Flymake syntax checking ..."))))))

(defun lazyflymake-guess-shell-script-regexp ()
  "Guess shell script file name regex."
  (let* ((ext (file-name-extension buffer-file-name)))
    (cond
     (ext
      (format "\\.%s$" ext))
     (t
      (format "\\%s$" (file-name-base buffer-file-name))))))

(defun lazyflymake--extract-err (output idx)
  "Extract error informationfrom OUTPUT using IDX."
  (cond
   (idx
    (match-string idx output))
   (t
    "nil")))

;;;###autoload
(defun lazyflymake-test-err-line-pattern ()
  "Test one line of command line progam output by `flymake-err-line-patterns'."
  (interactive)
  (let* ((output (read-string "One line of CLI output: "))
         (i 0)
         pattern
         len)
    (when (and output flymake-err-line-patterns)
      (setq len (length flymake-err-line-patterns))
      (while (< i len)
        (setq pattern (nth i flymake-err-line-patterns))
        (when (string-match (car pattern) output)
          (message "%d/%d matched: re=%s file=%s line=%s error=%s"
                   (1+ i)
                   len
                   (car pattern)
                   (lazyflymake--extract-err output (nth 1 pattern))
                   (lazyflymake--extract-err output (nth 2 pattern))
                   (lazyflymake--extract-err output (nth 4 pattern))))
        (setq i (1+ i))))))

;;;###autoload
(defun lazyflymake-start ()
  "Turn on lazyflymake to syntax check code."
  (interactive)

  ;; set up `flymake-allowed-file-name-masks'
  (lazyflymake-load "\\.el$" 'elisp)
  (lazyflymake-load "\\.lua$" 'lua)
  ;; a bit hard to get regex matching all shell script files
  (when (and (memq major-mode lazyflymake-shell-script-modes)
             (lazyflymake-sdk-file-exist-p))
    ;; File with `sh-mode' is shell script
    (lazyflymake-load (lazyflymake-guess-shell-script-regexp) 'shell))

  (if lazyflymake-debug (message "flymake-allowed-file-name-masks=%s" flymake-allowed-file-name-masks))

  (cond
   ;; for debug, unit test, and CI
   (lazyflymake-start-check-now
    (lazyflymake-start-now)
    (if lazyflymake-debug (message "Flymake syntax checking now ...")))

   (t
    ;; use global hook to save resource
    (add-hook 'after-save-hook #'lazyflymake-check-buffer nil))))

;;;###autoload
(defun lazyflymake-stop ()
  "Turn on lazyflymake to syntax check code."
  (interactive)
  (remove-hook 'after-save-hook #'lazyflymake-check-buffer nil))

(provide 'lazyflymake)
;;; lazyflymake.el ends here
