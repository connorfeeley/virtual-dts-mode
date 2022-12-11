;;; virtual-dts-mode.el --- Major mode for Device Tree binary files  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Connor Feeley
;;
;; Author: Connor Feeley <git@cfeeley.org>
;; Maintainer: Connor Feeley <git@cfeeley.org>
;; Created: December 8, 2022
;; Modified: December 10, 2022
;; Version: 0.2.1
;; Keywords: dts, languages, tools, unix, hardware
;; Homepage: https://sr.ht/~cfeeley/virtual-dts-mode/
;; Package-Requires: ((emacs "26.1") (dts-mode "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; This package provides an auto major mode to view device tree binary files
;; by decompiling them into their equivalent device tree source files.
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Code:

(defun virtual-dts-buffer (file)
  "Convert a `dtb' FILE to a `dts' buffer."
  ;; Invoke `dtc', ensuring all output is read
  (let* ((dtb-buffer (find-file-noselect file))
         (dtb-base-name (file-name-base file))
         (dts-buffer (get-buffer-create (format "*%s.dts" dtb-base-name)))
         (stdout (generate-new-buffer (format "*%s.dts<stdout>" dtb-base-name)))
         (stderr (generate-new-buffer (format "*%s.dts<stderr>" dtb-base-name)))
         (process (make-process :name "dtc"
                                :command (append (list "dtc" "-I" "dtb" "-O" "dts" (file-local-name (buffer-file-name dtb-buffer))))
                                :buffer stdout
                                :stderr stderr
                                :file-handler (lambda (process output)
                                                (with-current-buffer (process-buffer process)
                                                  (insert output)))))
         (stderr-process (get-buffer-process stderr)))

    ;; Don't include the "Process <name> finished" messages
    (set-process-sentinel process (lambda (process event) (message "Process %s has terminated: %s" process event)))
    (set-process-sentinel stderr-process (lambda (process event) (message "Process %s has terminated: %s" process event)))

    (unless (and process stderr-process) (error "Process unexpectedly nil"))
    (while (accept-process-output process))
    (while (accept-process-output stderr-process))

    ;; Ensure `read-only-mode' is off, and clear `dts-buffer' output from previous runs
    (with-current-buffer dts-buffer (read-only-mode 0) (erase-buffer))

    ;; Append `stdout' to `dts-buffer'
    (with-current-buffer stdout (append-to-buffer dts-buffer (point-min) (point-max)))

    ;; Append contents of `dtc-buffer' to `dts-buffer'
    (with-current-buffer stderr
      ;; Prepend `//' (comment) to each line of stderr from `dtc'
      (save-excursion
        (goto-char (point-min))
        (insert "\n")
        (while (not (eobp))
          (beginning-of-line)
          (insert "// ")
          (forward-line 1)))

      ;; Append the `dtc' warnings to `dts-buffer' as comments
      (append-to-buffer dts-buffer (point-min) (point-max)))

    ;; Delete the intermediate 'stdout' and `stderr' buffers
    (kill-buffer stdout)
    (kill-buffer stderr)

    ;; Return the `dts' buffer
    dts-buffer))

(defun virtual-dts ()
  "Decompile then view the equivalent `dts' for the current buffer."
  (interactive)

  ;; Generate the equivalent `dts' for the current `dtb' buffer and switch to it
  (switch-to-buffer (virtual-dts-buffer (buffer-file-name (current-buffer)))))

(define-derived-mode virtual-dts-mode
  dts-mode "Devicetree (virtual)"
  "Major mode for viewing the equivalent `dts' for a `dtb' file."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-buffer-substring (virtual-dts-buffer (buffer-file-name (current-buffer))))
    (set-buffer-modified-p nil)
    (read-only-mode 1))

  (add-hook 'change-major-mode-hook #'virtual-dts-mode-exit nil t))

(defun virtual-dts-mode-exit ()
  "Restore virtual-dts-mode when switching to another mode."

  (remove-hook 'change-major-mode-hook #'virtual-dts-mode-exit t)

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-buffer-modified-p nil)
    (read-only-mode nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '(".+\\.dtb\\|dtbo\\'" . virtual-dts-mode))

(provide 'virtual-dts-mode)
;;; virtual-dts-mode.el ends here
