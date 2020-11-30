;;; lazyflymake-tests.el ---  unit tests for lazyflymake -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

(require 'ert)
(require 'lazyflymake)

(setq lazyflymake-debug nil) ; debug
(setq lazyflymake-start-check-now t)

(defun lazyflymake-test-open-file (file)
  "Read FILE's content into current buffer."
  (let* ((files (directory-files-recursively default-directory file)))
    (when files
      (find-file (car files)))))

(ert-deftest lazyflymake-test-generic ()
  (let* ((lazyflymake-start-check-now t)
         (lazyflymake-flymake-mode-on nil))
    (lazyflymake-test-open-file "test-elisp.el")
    (goto-char (point-min))
    (lazyflymake-start)
    (flymake-goto-next-error)
    (should t)))

(ert-run-tests-batch-and-exit)
;;; lazyflymake-tests.el ends here
