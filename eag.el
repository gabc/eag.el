;;; eag.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023 gabc

;; Author: gabc
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Like awk, but more spicy.

;;; Code:
(require 'cl-lib)
(declare-function hash-table-values "subr-x.el")

(eval-and-compile
  (defun maprec (func tree)
    (mapcar (lambda (x)
              (if (listp x)
                  (maprec func x)
                (funcall func x)))
            tree))

  (defun eag--string-to-number (thing)
    "Returns either the number of the `thing' or the string if it is not a number."
    (if (string= "0" (string-trim thing))
        0
      (let ((res (string-to-number thing)))
        ;; We should not have a `0' here. so it must have been a string.
        (if (= res 0)
            thing
          res))))

  (let ((debug-on-error t))
    (dolist (test '((= 0 (eag--string-to-number "0"))
                    (= 12 (eag--string-to-number "12"))
                    (string= "asdf" (eag--string-to-number "asdf"))
                    ;; `string-to-number' returns 0 on a string.
                    ;; So we handle that explicitely.
                    (= 0 (string-to-number "asdfkj"))))
      (cl-assert (eval test)))))



(defmacro eag-match (&rest clauses)
  (let ((cleaned (maprec (lambda (n)
                           (if (symbolp n)
                               (let ((name (symbol-name n)))
                                 (if (and (> (length name) 1)
                                          (string-prefix-p "$" name))
                                     (list '$ (eag--string-to-number
                                               (string-replace "$" "" name)))
                                   n))
                             n))
                         clauses)))
    `(cond ,@cleaned)))

(defmacro eag--with-local-functions (&rest body)
  "Defines the local functions accessible in the eag body."
  `(cl-flet (($ (n)
               "Access field by number."
               (eag--string-to-number
                (if (and (stringp n) (string= n "NF"))
                    (car (last fields))
                  (if (= 0 n)
                      line
                    (nth (- n 1) fields)))))
             (set (var &optional val)
               (puthash (or val 0)
                        var vars))
             (add (var val)
               (if (gethash var vars)
                   (cl-typecase val
                     (integer (puthash var
                                       (+ (gethash var vars) val)
                                       vars))
                     (cons (puthash var
                                    (append (gethash var vars) val)
                                    vars)))
                 (puthash var val vars))))
     ,@body))

(defmacro eag (sep &rest body)
  (declare (indent 1))
  ;; Global variables
  `(let ((NL 0)
         (vars (make-hash-table :test 'equal)))
     (ignore NL vars)
     (goto-char 0)
     (while (not (eobp))
       ;; Local variables
       (let* ((line (buffer-substring-no-properties
                     (point)
                     (progn (forward-line 1)
                            (- (point) 1))))
              (fields (string-split line ,sep))
              (NF (length fields)))
         (ignore line fields NF)
         ;; Local functions
         (eag--with-local-functions
          (progn
            (eag-match
             ,@body)
            (cl-incf NL)))))
     vars))

(let ((debug-on-error t))
  (cl-assert (= 9
                (with-temp-buffer
                  (insert "1|2|3")
                  (newline)
                  (insert "2|4|3")
                  (gethash "res"
                           (eag "|" (t (add "res" (+ $1 $2))))))))
  (cl-assert (equal '(2 4)
                    (let (res)
                      (maphash #'(lambda (k v)
                                   (ignore k)
                                   (push v res))
                               (with-temp-buffer
                                 (insert "1|2|3")
                                 (newline)
                                 (insert "2|4|3")
                                 (eag "|" (t (add "res" (list $2))))))
                      (car res))))
  (cl-assert (equal '((3 3) (2 4))
                    (hash-table-values
                     (with-temp-buffer
                       (insert "1|2|3")
                       (newline)
                       (insert "2|4|3")
                       (newline)
                       (eag "|" (t (add "res" (list $2))
                                   (add "third" (list $3))))))))
  (cl-assert (equal '("e" 4)
                    (cl-flet ((res (table) (gethash "res" table)))
                      (res
                       (with-temp-buffer
                         (insert "a|e|3")
                         (newline)
                         (insert "2|4|3")
                         (eag "|" (t (add "res" (list $2)))))))))

  "Hardcoded $NF for the last field"
  (cl-assert (equal 3
                    (cl-flet ((res (table) (gethash "res" table)))
                      (res
                       (with-temp-buffer
                         (insert "a|e|3|1")
                         (newline)
                         (insert "0|4|2")
                         (newline)
                         (eag "|" (t (add "res" (+ $NF)))))))))

  "Make sure that we can access but-last field!
  Or arbitrary field, actually"
  (cl-assert (equal 7
                    (cl-flet ((res (table) (gethash "res" table)))
                      (res
                       (with-temp-buffer
                         (insert "a|e|3|1")
                         (newline)
                         (insert "0|4|2")
                         (newline)
                         (eag "|" (t (add "res"
                                          (+
                                           ($ (- NF 1))))))))))))


(provide 'eag)
;;; eag.el ends here