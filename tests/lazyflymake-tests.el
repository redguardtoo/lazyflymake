;;; evil-matchit-tests.el ---  unit tests for evil-matchit -*- coding: utf-8 -*-

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

(ert-deftest lazyflymake-test-generic ()
  (let* ((str "(message \"hello\")\n(message\n(message \"world\")\n(provide 'hello)\n;;; hello.el ends here"))
    (with-temp-buffer
      (insert str)
      (emacs-lisp-mode)
      (setq buffer-file-name "hello.el")
      (goto-char (point-min))
      (lazyflymake-start)
      (flymake-goto-next-error)
      (should t))))

(ert-run-tests-batch-and-exit)
;;; evil-matchit-tests.el ends here
