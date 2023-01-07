;; (decode-line "12,0 -> 4,5 -> 7,1")
;; => '((12 . 0) (4 . 5) (7 . 1))
(defun decode-line (line)
  (loop with chunks = (uiop:split-string line)
        until (null chunks)
        for (chunk _ . rest) = chunks
        do (setf chunks rest)
        collect (destructuring-bind (p1 p2)
                    (uiop:split-string chunk :separator '(#\,))
                  (cons (parse-integer p1)
                        (parse-integer p2)))))

(defun input ()
  (loop for line in (uiop:read-file-lines "~/cl/AOC/2022/day14/input.txt")
        collect (decode-line line)))

;; (trace-path '((101 . 5) (103 . 5) (103 . 3)))
;; => ((101 . 5) (102 . 5) (103 . 5) (103 . 3) (103 . 4) (103 . 5))
(defun trace-path (path)
  (loop for (x1 . y1) in path
        for (x2 . y2) in (cdr path)
        nconcing
        (loop for x from (min x1 x2) to (max x1 x2)
              nconcing
              (loop for y from (min y1 y2) to (max y1 y2)
                    collect (cons x y)))))

(defun build-cave (paths)
  (loop with cave = (make-hash-table :test #'equal)
        for pos in (loop for path in paths
                         nconcing (trace-path path))
        maximizing (cdr pos) into max-y
        do (setf (gethash pos cave) t)
        finally (return (cons cave max-y))))

(defun variant (x y cave)
  (let ((k (cons x y)))
    (and (not (gethash k cave)) k)))

(defun drop-one (on-bottom height cave)
  (loop named outer
        with pos = (cons 500 0)
        for (x . old-y) = pos
        if (gethash pos cave)
          do (return-from outer)
        if (> old-y height)
          do (return-from outer (funcall on-bottom pos cave))
        do (let* ((y (1+ old-y))
                  (new-pos (or (variant x y cave)
                               (variant (1- x) y cave)
                               (variant (1+ x) y cave))))
             (cond (new-pos (setf pos new-pos))
                   (t (setf (gethash pos cave) t)
                      (return-from outer t))))))

(defun go-to-the-void (pos cave)
  (declare (ignore pos cave))
  nil)

(defun stay-onto-the-floor (pos cave)
  (setf (gethash pos cave) t))

(defun solve-using (floor-handler inp)
  (destructuring-bind (cave . height) inp
    (loop while (drop-one floor-handler height cave)
          count t)))

(solve-using #'go-to-the-void (build-cave (input)))
;; => 1199

(solve-using #'stay-onto-the-floor (build-cave (input)))
;; => 23925
