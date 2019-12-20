(require 'subr-x)
(require 'seq)

(setq garbage-collection-messages t)
(setq gc-cons-threshold (* 64 1024 1024))

(defun day-2/make-input ()
  (vector 1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 1 6 19 1 19 6 23 2 23
          6 27 2 6 27 31 2 13 31 35 1 9 35 39 2 10 39 43 1 6 43
          47 1 13 47 51 2 6 51 55 2 55 6 59 1 59 5 63 2 9 63 67 1
          5 67 71 2 10 71 75 1 6 75 79 1 79 5 83 2 83 10 87 1 9
          87 91 1 5 91 95 1 95 6 99 2 10 99 103 1 5 103 107 1 107
          6 111 1 5 111 115 2 115 6 119 1 119 6 123 1 123 10 127
          1 127 13 131 1 131 2 135 1 135 5 0 99 2 14 0 0))

(defun day-2/intcode-store (xs loc val)
  (setf (elt xs loc) val)
  xs)

(defun day-2/intcode-load (xs loc)
  (elt xs (elt xs loc)))

(defun day-2/intcode-step (xs ip)
  (if (eq (elt xs ip) 99)
      (vector 'halt xs ip)
    (let ((x0 (elt xs ip))
          (x1 (day-2/intcode-load xs (+ 1 ip)))
          (x2 (day-2/intcode-load xs (+ 2 ip)))
          (x3 (elt xs (+ 3 ip))))
      (if (eq x0 1)
          (vector 'ok (day-2/intcode-store xs x3 (+ x1 x2)) (+ 4 ip))
        (vector 'ok (day-2/intcode-store xs x3 (* x1 x2)) (+ 4 ip))))))

(defun day-2/intcode-run-loop (state)
  (let ((ok/halt (elt state 0))
        (xs (elt state 1))
        (ip (elt state 2)))
    (if (eq ok/halt 'ok)
        (day-2/intcode-run-loop (day-2/intcode-step xs ip))
      xs)))

(defun day-2/intcode-run (xs)
  (day-2/intcode-run-loop (vector 'ok xs 0)))

(defun day-2/restore-noun+verb (xs noun verb)
  (day-2/intcode-store xs 1 noun)
  (day-2/intcode-store xs 2 verb)
  xs)

(defun day-2/intcode-restore+run (xs noun verb)
  (thread-first xs
    (day-2/restore-noun+verb noun verb)
    day-2/intcode-run
    (elt 0)))

;; (day-2/intcode-run [1 0 0 0 99])          ;; [2 0 0 0 99]
;; (day-2/intcode-run [2 3 0 3 99])          ;; [2 3 0 3 99]
;; (day-2/intcode-run [2 4 4 5 99 0])        ;; [2 4 4 5 99 0]
;; (day-2/intcode-run [1 1 1 4 99 5 6 0 99]) ;; [30 1 1 4 2 5 6 0 99]

(message "Day 2 / Part 1: %s"
         (day-2/intcode-restore+run (day-2/make-input) 12 2))

;; (defun day-2/find-noun+verb (nvs)
;;   (dolist (nv nvs answer)
;;     (pcase-let* ((`(,noun ,verb) nv)
;;                  (result (day-2/intcode-restore+run (day-2/make-input) noun verb)))
;;       (when (eq result 19690720)
;;         (setq answer (+ (* 100 noun) verb))))))

;; (defun day-2/find-noun+verb (nvs)
;;   (seq-first
;;    (seq-drop-while
;;     (lambda (nv)
;;       (pcase-let* ((`(,noun ,verb) nv)
;;                    (result (day-2/intcode-restore+run (day-2/make-input) noun verb)))
;;         (not (eq result 19690720))))
;;     nvs)))

(defun day-2/find-noun+verb (nvs)
  (seq-first
   (seq-drop-while
    (lambda (nv)
      (not (eq (day-2/intcode-restore+run (day-2/make-input) (car nv) (cdr nv)) 19690720)))
    nvs)))

;; (defun day-2/find-noun+verb ()
;;   (setq found? nil
;;         n -1)
;;   (while (and (not found?)
;;               (< n 99))
;;     (setq n (1+ n))
;;     (setq v -1)
;;     (while (and (not found?)
;;                 (< v 99))
;;       (setq v (1+ v))
;;       ;(print (list n v))
;;       (setq found?
;;             (eq 19690720
;;                 (day-2/intcode-restore+run (day-2/make-input) n v)))))
;;   (list found? n v))

(setq day-2/before (current-time))

(message "Day 2 / Part 2: %s"
         (day-2/find-noun+verb
          (mapcan (lambda (n) (mapcar (lambda (v) (cons n v))
                                      (number-sequence 0 99)))
                  (number-sequence 0 99))))

(setq day-2/after (current-time))

(message "%.2fms"
         (* 1000.0 (float-time (time-subtract day-2/after day-2/before))))
