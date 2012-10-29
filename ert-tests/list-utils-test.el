
(require 'list-utils)

;;; make-tconc

(ert-deftest make-tconc-01 nil
  (should (equal '[cl-struct-tconc nil nil]
                 (make-tconc))))

(ert-deftest make-tconc-02 nil
  (should (equal '[cl-struct-tconc (1 2 3) (3)]
                 (let ((lst '(1 2 3)))
                   (make-tconc :head lst :tail (last lst))))))


;;; tconc-list

(ert-deftest tconc-list-01 nil
  (should (equal '(1 2 3 4 5)
                 (let ((tc (make-tconc)))
                   (tconc-list tc '(1 2 3))
                   (tconc-list tc '(4 5))))))

(ert-deftest tconc-list-02 nil
  (should (equal '[cl-struct-tconc (1 2 3 4 5) (5)]
                 (let ((tc (make-tconc)))
                   (tconc-list tc '(1 2 3))
                   (tconc-list tc '(4 5))
                   tc))))


;;; tconc

(ert-deftest tconc-01 nil
  (should (equal '(1 2 3 4 5)
                 (let ((tc (make-tconc)))
                   (tconc tc 1 2 3)
                   (tconc tc 4 5)))))

(ert-deftest tconc-02 nil
  (should (equal '[cl-struct-tconc (1 2 3 4 5) (5)]
                 (let ((tc (make-tconc)))
                   (tconc tc 1 2 3)
                   (tconc tc 4 5)
                   tc))))


;;; list-utils-cons-cell-p

(ert-deftest list-utils-cons-cell-p-01 nil
  (should-not
   (list-utils-cons-cell-p '(a b c d e f))))

(ert-deftest list-utils-cons-cell-p-02 nil
  (should-not
   (list-utils-cons-cell-p nil)))

(ert-deftest list-utils-cons-cell-p-03 nil
  (should-not
   (list-utils-cons-cell-p 1)))

(ert-deftest list-utils-cons-cell-p-04 nil
  (should (= 2
             (list-utils-cons-cell-p '(1 . 2)))))

(ert-deftest list-utils-cons-cell-p-05 nil
  (should (= 6
             (list-utils-cons-cell-p '(1 2 3 4 5 . 6)))))


;;; list-utils-make-proper

(ert-deftest list-utils-make-proper-01 nil
  (should (equal '(a b c d e f)
                 (list-utils-make-proper '(a b c d e f)))))

(ert-deftest list-utils-make-proper-02 nil
  (should-not
   (list-utils-make-proper nil)))

(ert-deftest list-utils-make-proper-03 nil
  (should-error
   (list-utils-make-proper 1)))

(ert-deftest list-utils-make-proper-04 nil
  (should (equal '(1 2)
                 (list-utils-make-proper '(1 . 2)))))

(ert-deftest list-utils-make-proper-05 nil
  (should (equal '(1 2 3 4 5 6)
                 (list-utils-make-proper '(1 2 3 4 5 . 6)))))


;;; list-utils-make-improper

(ert-deftest list-utils-make-improper-01 nil
  (should (equal '(a b c d e . f)
                 (list-utils-make-improper '(a b c d e f)))))

(ert-deftest list-utils-make-improper-02 nil
  (should-error
   (list-utils-make-improper nil)))

(ert-deftest list-utils-make-improper-03 nil
  (should-error
   (list-utils-make-improper 1)))

(ert-deftest list-utils-make-improper-04 nil
  (should (equal '(1 . 2)
                 (list-utils-make-improper '(1 . 2)))))

(ert-deftest list-utils-make-improper-05 nil
  (should (equal '(1 2 3 4 5 . 6)
                 (list-utils-make-improper '(1 2 3 4 5 . 6)))))

(ert-deftest list-utils-make-improper-06 nil
  (should-error
   (list-utils-make-improper '(1))))


;;; list-utils-cyclic-length

(ert-deftest list-utils-cyclic-length-01 nil
  (should (= 8
             (let ((cyclic '(a b c d e f g h)))
               (nconc cyclic cyclic)
               (list-utils-cyclic-length cyclic)))))

(ert-deftest list-utils-cyclic-length-02 nil
  (should (= 7
             (let ((cyclic '(a b c d e f g h)))
               (nconc cyclic (cdr cyclic))
               (list-utils-cyclic-length cyclic)))))

(ert-deftest list-utils-cyclic-length-03 nil
  (should (= 1
             (let ((cyclic '(a b c d e f g h)))
               (nconc cyclic (last cyclic))
               (list-utils-cyclic-length cyclic)))))

(ert-deftest list-utils-cyclic-length-04 nil
  (should (= 0
             (list-utils-cyclic-length (cons 1 2)))))

(ert-deftest list-utils-cyclic-length-05 nil
  (should (= 0
             (list-utils-cyclic-length (list* 1 2 3)))))

(ert-deftest list-utils-cyclic-length-06 nil
  (let ((cyclic '(1)))
    (nconc cyclic cyclic)
    (should (= 1
               (list-utils-cyclic-length cyclic)))))


;;; list-utils-cyclic-subseq

(ert-deftest list-utils-cyclic-subseq-01 nil
  (should (equal '(1 2 3 4 5 6 7 8)
                 (let ((cyclic '(1 2 3 4 5 6 7 8)))
                   (nconc cyclic cyclic)
                   (sort (list-utils-flatten (list-utils-cyclic-subseq cyclic)) '<)))))

(ert-deftest list-utils-cyclic-subseq-02 nil
  (should (equal '(2 3 4 5 6 7 8)
                 (let ((cyclic '(1 2 3 4 5 6 7 8)))
                   (nconc cyclic (cdr cyclic))
                   (sort (list-utils-flatten (list-utils-cyclic-subseq cyclic)) '<)))))

(ert-deftest list-utils-cyclic-subseq-03 nil
  (should (equal '(2 3 4 5 6 7 8)
                 (let ((cyclic '(1 2 3 4 5 6 7 8)))
                   (nconc cyclic (cdr cyclic))
                   (list-utils-flatten (list-utils-cyclic-subseq cyclic 'from-start))))))

(ert-deftest list-utils-cyclic-subseq-04 nil
  (should (equal '(8)
                 (let ((cyclic '(1 2 3 4 5 6 7 8)))
                   (nconc cyclic (last cyclic))
                   (list-utils-flatten (list-utils-cyclic-subseq cyclic))))))

(ert-deftest list-utils-cyclic-subseq-05 nil
  (should-not
   (list-utils-cyclic-subseq '(1 2 3))))

(ert-deftest list-utils-cyclic-subseq-06 nil
  (should-not
   (list-utils-cyclic-subseq nil)))

(ert-deftest list-utils-cyclic-subseq-07 nil
  (should-not
   (list-utils-cyclic-subseq (cons 1 2))))

(ert-deftest list-utils-cyclic-subseq-08 nil
  (should-not
   (list-utils-cyclic-subseq (list* 1 2 3))))

(ert-deftest list-utils-cyclic-subseq-09 nil
  (let ((cyclic '(1)))
    (nconc cyclic cyclic)
    (should (equal '(1)
                   (list-utils-flatten (list-utils-cyclic-subseq cyclic))))))



;;; list-utils-cyclic-p

(ert-deftest list-utils-cyclic-p-01 nil
  (should
   (let ((cyclic '(1 2 3 4 5 6 7 8)))
     (nconc cyclic cyclic)
     (list-utils-cyclic-p cyclic))))

(ert-deftest list-utils-cyclic-p-02 nil
  (should
   (let ((cyclic '(1 2 3 4 5 6 7 8)))
     (nconc cyclic cyclic)
     (list-utils-cyclic-p cyclic 'perfect))))

(ert-deftest list-utils-cyclic-p-03 nil
  (should
   (let ((cyclic '(1 2 3 4 5 6 7 8)))
     (nconc cyclic (cdr cyclic))
     (list-utils-cyclic-p cyclic))))

(ert-deftest list-utils-cyclic-p-04 nil
  (should-not
   (let ((cyclic '(1 2 3 4 5 6 7 8)))
     (nconc cyclic (cdr cyclic))
     (list-utils-cyclic-p cyclic 'perfect))))

(ert-deftest list-utils-cyclic-p-05 nil
  (should
   (let ((cyclic '(1 2 3 4 5 6 7 8)))
     (nconc cyclic (last cyclic))
     (list-utils-cyclic-p cyclic))))

(ert-deftest list-utils-cyclic-p-06 nil
  (should-not
   (list-utils-cyclic-p '(1 2 3))))

(ert-deftest list-utils-cyclic-p-07 nil
  (should-not
   (list-utils-cyclic-p nil)))

(ert-deftest list-utils-cyclic-p-08 nil
  (should-not
   (list-utils-cyclic-p (cons 1 2))))

(ert-deftest list-utils-cyclic-p-09 nil
  (should-not
   (list-utils-cyclic-p (list* 1 2 3))))

(ert-deftest list-utils-cyclic-p-10 nil
  (should
   (let ((cyclic '(1)))
     (nconc cyclic cyclic)
     (list-utils-cyclic-p cyclic))))


;;; list-utils-linear-p

(ert-deftest list-utils-linear-p-01 nil
  (should-not
   (let ((cyclic '(1 2 3 4 5 6 7 8)))
     (nconc cyclic cyclic)
     (list-utils-linear-p cyclic))))

(ert-deftest list-utils-linear-p-02 nil
  (should-not
   (let ((cyclic '(1 2 3 4 5 6 7 8)))
     (nconc cyclic (cdr cyclic))
     (list-utils-linear-p cyclic))))

(ert-deftest list-utils-linear-p-03 nil
  (should-not
   (let ((cyclic '(1 2 3 4 5 6 7 8)))
     (nconc cyclic (last cyclic))
     (list-utils-linear-p cyclic))))

(ert-deftest list-utils-linear-p-04 nil
  (should
   (list-utils-linear-p '(1 2 3))))

(ert-deftest list-utils-linear-p-05 nil
  (should
   (list-utils-linear-p nil)))

(ert-deftest list-utils-linear-p-06 nil
  (should
   (list-utils-linear-p (cons 1 2))))

(ert-deftest list-utils-linear-p-07 nil
  (should
   (list-utils-linear-p (list* 1 2 3))))

(ert-deftest list-utils-linear-p-08 nil
  (let ((cyclic '(1)))
    (nconc cyclic cyclic)
    (should-not
     (list-utils-linear-p cyclic))))


;;; list-utils-linear-subseq

(ert-deftest list-utils-linear-subseq-01 nil
  (should-not
   (let ((cyclic '(a b c d e f g h)))
     (nconc cyclic cyclic)
     (list-utils-linear-subseq cyclic))))

(ert-deftest list-utils-linear-subseq-02 nil
  (should (equal '(a)
                 (let ((cyclic '(a b c d e f g h)))
                   (nconc cyclic (cdr cyclic))
                   (list-utils-linear-subseq cyclic)))))

(ert-deftest list-utils-linear-subseq-03 nil
  (should (equal '(a b c d e f g)
                 (let ((cyclic '(a b c d e f g h)))
                   (nconc cyclic (last cyclic))
                   (list-utils-linear-subseq cyclic)))))

(ert-deftest list-utils-linear-subseq-04 nil
  (let ((improper (cons 1 2)))
    (should (equal improper
                   (list-utils-linear-subseq improper)))))

(ert-deftest list-utils-linear-subseq-05 nil
  (let ((improper (list* 1 2 3)))
    (should (equal improper
                   (list-utils-linear-subseq (list* 1 2 3))))))

(ert-deftest list-utils-linear-subseq-06 nil
  (let ((cyclic '(1)))
    (nconc cyclic cyclic)
    (should-not
     (list-utils-linear-subseq cyclic))))


;;; list-utils-safe-length

(ert-deftest list-utils-safe-length-01 nil
  (should (= 8
             (let ((cyclic '(a b c d e f g h)))
               (nconc cyclic cyclic)
               (list-utils-safe-length cyclic)))))

(ert-deftest list-utils-safe-length-02 nil
  (should (= 8
             (let ((cyclic '(a b c d e f g h)))
               (nconc cyclic (cdr cyclic))
               (list-utils-safe-length cyclic)))))

(ert-deftest list-utils-safe-length-03 nil
  (should (= 8
             (let ((cyclic '(a b c d e f g h)))
               (nconc cyclic (last cyclic))
               (list-utils-safe-length cyclic)))))

(ert-deftest list-utils-safe-length-04 nil
  (should (= 8
             (let ((cyclic '(a b c d e f g h)))
               (list-utils-safe-length cyclic)))))

(ert-deftest list-utils-safe-length-05 nil
  (should (= 0
             (list-utils-safe-length nil))))

(ert-deftest list-utils-safe-length-06 nil
  (should (= 0
             (list-utils-safe-length :not-a-list))))

(ert-deftest list-utils-safe-length-07 nil
  (should (= 1
             (list-utils-safe-length (cons 1 2)))))

(ert-deftest list-utils-safe-length-08 nil
  (should (= 2
             (list-utils-safe-length (list* 1 2 3)))))

(ert-deftest list-utils-safe-length-09 nil
  (let ((cyclic '(1)))
    (nconc cyclic cyclic)
    (should (= 1
               (list-utils-safe-length cyclic)))))


;;; list-utils-flatten

(ert-deftest list-utils-flatten-01 nil
  (should (equal '(a b c d e f)
                 (list-utils-flatten '(a b c (d e (f)))))))

(ert-deftest list-utils-flatten-02 nil
  (should (equal '(a nil b nil c nil d nil e nil f nil nil nil)
                 (list-utils-flatten '(a nil b nil c nil (d nil e nil (f nil) nil) nil)))))

(ert-deftest list-utils-flatten-03 nil
  (should (equal '(1 2 3 4 5)
                 (list-utils-flatten '(1 2 3 4 . 5)))))

(ert-deftest list-utils-flatten-04 nil
  (should (equal '(1 2 3 4 5)
                 (list-utils-flatten '(1 2 3 (4 . 5))))))

(ert-deftest list-utils-flatten-05 nil
  (should (equal '(1 2 3 4 5)
                 (let ((cyclic '(1 2 3 (4 5))))
                   (nconc cyclic (cdr cyclic))
                   (list-utils-flatten cyclic)))))

(ert-deftest list-utils-flatten-06 nil
  (should (equal '(1 2)
   (list-utils-flatten (cons 1 2)))))

(ert-deftest list-utils-flatten-07 nil
  (let ((cyclic '(1)))
    (nconc cyclic cyclic)
    (should (equal '(1)
                   (list-utils-flatten cyclic)))))

(ert-deftest list-utils-flatten-08 nil
  "Don't modifiy LIST"
  (let ((cyclic-1 '(1 2 3 (4 5)))
        (cyclic-2 '(1 2 3 (4 5))))
    (nconc cyclic-1 (cdr cyclic-1))
    (nconc cyclic-2 (cdr cyclic-2))
    (should
     (equal '(1 2 3 4 5)
        (list-utils-flatten cyclic-1)))
    (should
     (equal (list-utils-linear-subseq cyclic-1)
            (list-utils-linear-subseq cyclic-2)))
    (should
     (equal
      (subseq (list-utils-cyclic-subseq cyclic-1) 0 (list-utils-safe-length (list-utils-cyclic-subseq cyclic-1)))
      (subseq (list-utils-cyclic-subseq cyclic-2) 0 (list-utils-safe-length (list-utils-cyclic-subseq cyclic-2)))))))


;;; list-utils-depth

(ert-deftest list-utils-depth-01 nil
  (should
   (= 0
      (list-utils-depth nil))))

(ert-deftest list-utils-depth-02 nil
  (should
   (= 0
      (list-utils-depth "not a list"))))

(ert-deftest list-utils-depth-03 nil
  (should
   (= 1
      (list-utils-depth '(1 2 3)))))

(ert-deftest list-utils-depth-04 nil
  (should
   (= 1
      (list-utils-depth (cons 1 2)))))

(ert-deftest list-utils-depth-05 nil
  (should
   (= 3
      (list-utils-depth '(a b c (d e (f)))))))

(ert-deftest list-utils-depth-06 nil
  (should
   (= 3
      (list-utils-depth '(a nil b nil c nil (d nil e nil (f nil) nil) nil)))))

(ert-deftest list-utils-depth-07 nil
  (should
   (= 1
      (list-utils-depth '(1 2 3 4 . 5)))))

(ert-deftest list-utils-depth-08 nil
  (should
   (= 2
      (list-utils-depth '(1 2 3 (4 . 5))))))

(ert-deftest list-utils-depth-09 nil
  (should
   (= 1
      (let ((cyclic '(1 2 3 4 5)))
        (nconc cyclic (cdr cyclic))
        (list-utils-depth cyclic)))))

(ert-deftest list-utils-depth-10 nil
  (should
   (= 2
      (let ((cyclic '(1 2 3 (4 5))))
        (nconc cyclic (cdr cyclic))
        (list-utils-depth cyclic)))))

(ert-deftest list-utils-depth-11 nil
  (let* ((value '(a nil (b . 1) nil (c 2 . 3) nil (d nil e nil (f nil) nil) nil))
         (copy (copy-tree value)))
    (list-utils-depth value)
    (should
     (equal value copy))))

(ert-deftest list-utils-depth-12 nil
  (let ((cyclic '(1)))
    (nconc cyclic cyclic)
    (should (= 1
               (list-utils-depth cyclic)))))

(ert-deftest list-utils-depth-13 nil
  "Don't modifiy LIST"
  (let ((cyclic-1 '(1 2 3 (4 5)))
        (cyclic-2 '(1 2 3 (4 5))))
    (nconc cyclic-1 (cdr cyclic-1))
    (nconc cyclic-2 (cdr cyclic-2))
    (should
     (= 2
        (list-utils-depth cyclic-1)))
    (should
     (equal (list-utils-linear-subseq cyclic-1)
            (list-utils-linear-subseq cyclic-2)))
    (should
     (equal
      (subseq (list-utils-cyclic-subseq cyclic-1) 0 (list-utils-safe-length (list-utils-cyclic-subseq cyclic-1)))
      (subseq (list-utils-cyclic-subseq cyclic-2) 0 (list-utils-safe-length (list-utils-cyclic-subseq cyclic-2)))))))


;;; list-utils-alist-flatten

(ert-deftest list-utils-alist-flatten-01 nil
  (should (equal '(1 2 3 4 . 5)
                 (list-utils-alist-flatten '(1 2 3 4 . 5)))))

(ert-deftest list-utils-alist-flatten-02 nil
  (should (equal '(1 2 3 (4 . 5))
                 (list-utils-alist-flatten '(1 2 3 (4 . 5))))))

(ert-deftest list-utils-alist-flatten-03 nil
  (should (equal '(1 2 3 (4 . 5))
                 (list-utils-alist-flatten '(1 (2 3) (4 . 5))))))

(ert-deftest list-utils-alist-flatten-04 nil
  (should (equal '((1 . 2) (3 . 4) (5 . 6) (7 . 8))
                 (list-utils-alist-flatten '(((1 . 2) (3 . 4)) ((5 . 6) (7 . 8)))))))

(ert-deftest list-utils-alist-flatten-05 nil
  (should (equal (cons 1 2)
                 (list-utils-alist-flatten (cons 1 2)))))

(ert-deftest list-utils-alist-flatten-06 nil
  "Don't modifiy LIST"
  (let ((cyclic-1 '(1 2 3 ((4 . 5) (6 . 7))))
        (cyclic-2 '(1 2 3 ((4 . 5) (6 . 7)))))
    (nconc cyclic-1 (cdr cyclic-1))
    (nconc cyclic-2 (cdr cyclic-2))
    (should
     (equal '(1 2 3 (4 . 5) (6 . 7))
        (list-utils-alist-flatten cyclic-1)))
    (should
     (equal (list-utils-linear-subseq cyclic-1)
            (list-utils-linear-subseq cyclic-2)))
    (should
     (equal
      (subseq (list-utils-cyclic-subseq cyclic-1) 0 (list-utils-safe-length (list-utils-cyclic-subseq cyclic-1)))
      (subseq (list-utils-cyclic-subseq cyclic-2) 0 (list-utils-safe-length (list-utils-cyclic-subseq cyclic-2)))))))


;;; list-utils-insert-before

(ert-deftest list-utils-insert-before-01 nil
  (should (equal '(1 2 3 four 4 5)
                 (list-utils-insert-before '(1 2 3 4 5) 4 'four))))

(ert-deftest list-utils-insert-before-02 nil
  (should (equal '(elt 1 2 3 4 5)
                 (list-utils-insert-before '(1 2 3 4 5) 1 'elt))))

(ert-deftest list-utils-insert-before-03 nil
  (should (equal '(1 2 3 4 elt 5)
                 (list-utils-insert-before '(1 2 3 4 5) 5 'elt))))

(ert-deftest list-utils-insert-before-04 nil
  (should-error
   (list-utils-insert-before '(1 2 3 4 5) 6 'elt)))

(ert-deftest list-utils-insert-before-05 nil
  (should (equal (list* 'elt 1 2)
             (list-utils-insert-before (cons 1 2) 1 'elt))))

(ert-deftest list-utils-insert-before-06 nil
  (should (equal (list* 1 'elt 2)
                 (list-utils-insert-before (cons 1 2) 2 'elt))))

(ert-deftest list-utils-insert-before-07 nil
  (should (equal (list* 1 'elt 2 3)
                 (list-utils-insert-before (list* 1 2 3) 2 'elt))))

(ert-deftest list-utils-insert-before-08 nil
  (should (equal (list* 1 2 'elt 3)
                 (list-utils-insert-before (list* 1 2 3) 3 'elt))))


;;; list-utils-insert-after

(ert-deftest list-utils-insert-after-01 nil
  (should (equal '(1 2 3 4 four 5)
                 (list-utils-insert-after '(1 2 3 4 5) 4 'four))))

(ert-deftest list-utils-insert-after-02 nil
  (should (equal '(1 elt 2 3 4 5)
                 (list-utils-insert-after '(1 2 3 4 5) 1 'elt))))

(ert-deftest list-utils-insert-after-03 nil
  (should (equal '(1 2 3 4 5 elt)
                 (list-utils-insert-after '(1 2 3 4 5) 5 'elt))))

(ert-deftest list-utils-insert-after-04 nil
  (should-error
   (list-utils-insert-after '(1 2 3 4 5) 6 'elt)))

(ert-deftest list-utils-insert-after-05 nil
  (should (equal (list* 1 'elt 2)
                 (list-utils-insert-after (cons 1 2) 1 'elt))))

(ert-deftest list-utils-insert-after-06 nil
  (should (equal (list* 1 2 'elt)
                 (list-utils-insert-after (cons 1 2) 2 'elt))))

(ert-deftest list-utils-insert-after-07 nil
  (should (equal (list* 1 2 'elt 3)
                 (list-utils-insert-after (list* 1 2 3) 2 'elt))))

(ert-deftest list-utils-insert-after-08 nil
  (should (equal (list* 1 2 3 'elt)
                 (list-utils-insert-after (list* 1 2 3) 3 'elt))))


;;; list-utils-insert-before-pos

(ert-deftest list-utils-insert-before-pos-01 nil
  (should (equal '(a b c three d e)
                 (list-utils-insert-before-pos '(a b c d e) 3 'three))))

(ert-deftest list-utils-insert-before-pos-02 nil
  (should (equal '(elt a b c d e)
                 (list-utils-insert-before-pos '(a b c d e) 0 'elt))))

(ert-deftest list-utils-insert-before-pos-03 nil
  (should (equal '(a b c d elt e)
                 (list-utils-insert-before-pos '(a b c d e) 4 'elt))))

(ert-deftest list-utils-insert-before-pos-04 nil
  (should-error
   (list-utils-insert-before-pos '(a b c d e) 5 'elt)))

(ert-deftest list-utils-insert-before-pos-05 nil
  (should (equal (list* 'elt 1 2)
                 (list-utils-insert-before-pos (cons 1 2) 0 'elt))))

(ert-deftest list-utils-insert-before-pos-06 nil
  (should (equal (list* 1 'elt 2)
                 (list-utils-insert-before-pos (cons 1 2) 1 'elt))))

(ert-deftest list-utils-insert-before-pos-07 nil
  (should-error
   (list-utils-insert-before-pos (cons 1 2) 2 'elt)))

(ert-deftest list-utils-insert-before-pos-08 nil
  (should (equal (list* 1 'elt 2 3)
                 (list-utils-insert-before-pos (list* 1 2 3) 1 'elt))))

(ert-deftest list-utils-insert-before-pos-09 nil
  (should (equal (list* 1 2 'elt 3)
                 (list-utils-insert-before-pos (list* 1 2 3) 2 'elt))))

(ert-deftest list-utils-insert-before-pos-10 nil
  (should-error
   (list-utils-insert-before-pos (list* 1 2 3) 3 'elt)))


;;; list-utils-insert-after-pos

(ert-deftest list-utils-insert-after-pos-01 nil
  (should (equal '(a b c d three e)
                 (list-utils-insert-after-pos '(a b c d e) 3 'three))))

(ert-deftest list-utils-insert-after-pos-02 nil
  (should (equal '(a elt b c d e)
                 (list-utils-insert-after-pos '(a b c d e) 0 'elt))))

(ert-deftest list-utils-insert-after-pos-03 nil
  (should (equal '(a b c d e elt)
                 (list-utils-insert-after-pos '(a b c d e) 4 'elt))))

(ert-deftest list-utils-insert-after-pos-04 nil
  (should-error
   (list-utils-insert-after-pos '(a b c d e) 5 'elt)))

(ert-deftest list-utils-insert-after-pos-05 nil
  (should (equal (list* 1 'elt 2)
                 (list-utils-insert-after-pos (cons 1 2) 0 'elt))))

(ert-deftest list-utils-insert-after-pos-06 nil
  (should (equal (list* 1 2 'elt)
                 (list-utils-insert-after-pos (cons 1 2) 1 'elt))))

(ert-deftest list-utils-insert-after-pos-07 nil
  (should-error
   (list-utils-insert-after-pos (cons 1 2) 2 'elt)))

(ert-deftest list-utils-insert-after-pos-08 nil
  (should (equal (list* 1 2 'elt 3)
                 (list-utils-insert-after-pos (list* 1 2 3) 1 'elt))))

(ert-deftest list-utils-insert-after-pos-09 nil
  (should (equal (list* 1 2 3 'elt)
                 (list-utils-insert-after-pos (list* 1 2 3) 2 'elt))))

(ert-deftest list-utils-insert-after-pos-10 nil
  (should-error
   (list-utils-insert-after-pos (list* 1 2 3) 3 'elt)))


;;; list-utils-plist-reverse

(ert-deftest list-utils-plist-reverse-01 nil
  (should (equal '(:four 4 :three 3 :two 2 :one 1)
                 (list-utils-plist-reverse '(:one 1 :two 2 :three 3 :four 4)))))

(ert-deftest list-utils-plist-reverse-02 nil
  (should-error
   (list-utils-plist-reverse '(:one 1 :two 2 :three 3 :four))))


;;; list-utils-plist-del

(ert-deftest list-utils-plist-del-01 nil
  (should (equal '(:one 1 :two 2 :three 3 :four 4)
                 (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) :six))))

(ert-deftest list-utils-plist-del-02 nil
  (should (equal '(:one 1 :two 2 :three 3 :four 4)
                 (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) 4))))

(ert-deftest list-utils-plist-del-03 nil
  (should (equal '(:one 1 :two 2 :three 3 :four 4)
                 (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) 2))))

(ert-deftest list-utils-plist-del-04 nil
  (should (equal '(:one 1 :three 3 :four 4)
                 (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) :two))))

(ert-deftest list-utils-plist-del-05 nil
  (should (equal '(:two 2 :three 3 :four 4)
                 (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) :one))))

(ert-deftest list-utils-plist-del-06 nil
  (should (equal '(:one 1 :two 2 :three 3)
                 (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) :four))))

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; list-utils-test.el ends here
