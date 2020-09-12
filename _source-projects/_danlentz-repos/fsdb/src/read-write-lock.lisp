; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A simple read-write-lock
;;;

(in-package :fsdb)

;; Maybe this should use lists of processes instead of counters.
;; This is a little faster and conses less (not at all), but is more
;; brittle.
(defstruct read-write-lock
  (lock (fsdb:make-lock))
  (readers 0)
  (write-waiting-p nil)
  (write-semaphore (make-semaphore)))

(defun read-lock-rwlock (lock)
  (loop
     (with-lock-grabbed ((read-write-lock-lock lock))
       (unless (read-write-lock-write-waiting-p lock)
         (incf (read-write-lock-readers lock))
         (return)))
     (process-wait
      "read-lock-rwlock"
      (lambda (lock)
        (not (read-write-lock-write-waiting-p lock)))
      lock)))

(defun read-unlock-rwlock (lock)
  (with-lock-grabbed ((read-write-lock-lock lock))
    (assert (not (eql 0 (read-write-lock-readers lock))))
    (when (eql 0 (decf (read-write-lock-readers lock)))
      (when (read-write-lock-write-waiting-p lock)
        (signal-semaphore
         (read-write-lock-write-semaphore lock))))))

(defun write-lock-rwlock (lock &optional reading-p)
  (let ((wait-p nil))
    (with-lock-grabbed ((read-write-lock-lock lock))
      (assert (not (read-write-lock-write-waiting-p lock)))
      (setf (read-write-lock-write-waiting-p lock) t)
      (when reading-p
        (decf (read-write-lock-readers lock)))
      (unless (eql 0 (read-write-lock-readers lock))
        (setf wait-p t)))
    (when wait-p
      (wait-on-semaphore
       (read-write-lock-write-semaphore lock)))))

(defun write-unlock-rwlock (lock &optional reading-p)
  (with-lock-grabbed ((read-write-lock-lock lock))
    (setf (read-write-lock-write-waiting-p lock) nil)
    (when reading-p
      (incf (read-write-lock-readers lock)))))

(defun unlock-rwlock (lock)
  (if (read-write-lock-write-waiting-p lock)
      (write-unlock-rwlock lock)
      (read-unlock-rwlock lock)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2010 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
