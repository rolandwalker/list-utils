(require 'list-utils)

(expectations

  (desc "make-tconc")

  (expect '[cl-struct-tconc nil nil]
    (make-tconc))

  (expect '[cl-struct-tconc (1 2 3) (3)]
    (let ((lst '(1 2 3)))
      (make-tconc :head lst :tail (last lst)))))


(expectations

  (desc "tconc-list")

  (expect '(1 2 3 4 5)
    (let ((tc (make-tconc)))
      (tconc-list tc '(1 2 3))
      (tconc-list tc '(4 5))))

  (expect '[cl-struct-tconc (1 2 3 4 5) (5)]
    (let ((tc (make-tconc)))
      (tconc-list tc '(1 2 3))
      (tconc-list tc '(4 5))
      tc)))


(expectations

  (desc "tconc")

  (expect '(1 2 3 4 5)
    (let ((tc (make-tconc)))
      (tconc tc 1 2 3)
      (tconc tc 4 5)))

  (expect '[cl-struct-tconc (1 2 3 4 5) (5)]
    (let ((tc (make-tconc)))
      (tconc tc 1 2 3)
      (tconc tc 4 5)
      tc)))


(expectations

  (desc "list-utils-cons-cell-p")

  (expect nil
    (list-utils-cons-cell-p '(a b c d e f)))

  (expect nil
    (list-utils-cons-cell-p nil))

  (expect nil
    (list-utils-cons-cell-p 1))

  (expect 2
    (list-utils-cons-cell-p '(1 . 2)))

  (expect 6
    (list-utils-cons-cell-p '(1 2 3 4 5 . 6))))


(expectations

  (desc "list-utils-cyclic-length")

  (expect 8
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic cyclic)
      (list-utils-cyclic-length cyclic)))

  (expect 7
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic (cdr cyclic))
      (list-utils-cyclic-length cyclic)))

  (expect 1
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic (last cyclic))
      (list-utils-cyclic-length cyclic))))

(expectations

  (desc "list-utils-cyclic-subseq")

  (expect '(1 2 3 4 5 6 7 8)
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic cyclic)
      (sort (list-utils-flatten (list-utils-cyclic-subseq cyclic)) '<)))

  (expect '(2 3 4 5 6 7 8)
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic (cdr cyclic))
      (sort (list-utils-flatten (list-utils-cyclic-subseq cyclic)) '<)))

  (expect '(2 3 4 5 6 7 8)
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic (cdr cyclic))
      (list-utils-flatten (list-utils-cyclic-subseq cyclic 'from-start))))

  (expect '(8)
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic (last cyclic))
      (list-utils-flatten (list-utils-cyclic-subseq cyclic))))

  (expect nil
    (list-utils-cyclic-subseq '(1 2 3)))

  (expect nil
    (list-utils-cyclic-subseq nil)))


(expectations

  (desc "list-utils-cyclic-p")

  (expect t
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic cyclic)
      (and (list-utils-cyclic-p cyclic) t)))

  (expect t
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic cyclic)
      (and (list-utils-cyclic-p cyclic 'perfect) t)))

  (expect t
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic (cdr cyclic))
      (and (list-utils-cyclic-p cyclic) t)))

  (expect nil
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic (cdr cyclic))
      (and (list-utils-cyclic-p cyclic 'perfect) t)))

  (expect t
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic (last cyclic))
      (and (list-utils-cyclic-p cyclic) t)))

  (expect nil
    (list-utils-cyclic-p '(1 2 3)))

  (expect nil
    (list-utils-cyclic-p nil)))


(expectations

  (desc "list-utils-linear-p")

  (expect nil
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic cyclic)
      (list-utils-linear-p cyclic)))

  (expect nil
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic (cdr cyclic))
      (list-utils-linear-p cyclic)))

  (expect nil
    (let ((cyclic '(1 2 3 4 5 6 7 8)))
      (nconc cyclic (last cyclic))
      (list-utils-linear-p cyclic)))

  (expect t
    (list-utils-linear-p '(1 2 3)))

  (expect t
    (list-utils-linear-p nil)))


(expectations

  (desc "list-utils-linear-subseq")

  (expect nil
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic cyclic)
      (list-utils-linear-subseq cyclic)))

  (expect '(a)
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic (cdr cyclic))
      (list-utils-linear-subseq cyclic)))

  (expect '(a b c d e f g)
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic (last cyclic))
      (list-utils-linear-subseq cyclic))))

(expectations

  (desc "list-utils-safe-length")

  (expect 8
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic cyclic)
      (list-utils-safe-length cyclic)))

  (expect 8
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic (cdr cyclic))
      (list-utils-safe-length cyclic)))

  (expect 8
    (let ((cyclic '(a b c d e f g h)))
      (nconc cyclic (last cyclic))
      (list-utils-safe-length cyclic)))

  (expect 8
    (let ((cyclic '(a b c d e f g h)))
      (list-utils-safe-length cyclic)))

  (expect 0
    (list-utils-safe-length nil))

  (expect 0
    (list-utils-safe-length :not-a-list)))


(expectations

  (desc "list-utils-flatten")

  (expect '(a b c d e f)
    (list-utils-flatten '(a b c (d e (f)))))

  (expect '(a nil b nil c nil d nil e nil f nil nil nil)
    (list-utils-flatten '(a nil b nil c nil (d nil e nil (f nil) nil) nil)))

  (expect '(1 2 3 4 5)
    (list-utils-flatten '(1 2 3 4 . 5)))

  (expect '(1 2 3 4 5)
    (list-utils-flatten '(1 2 3 (4 . 5))))

  (expect '(1 2 3 4 5)
    (let ((cyclic '(1 2 3 (4 5))))
      (nconc cyclic (cdr cyclic))
      (list-utils-flatten cyclic))))


(expectations

  (desc "list-utils-alist-flatten")

  (expect '(1 2 3 4 . 5)
    (list-utils-alist-flatten '(1 2 3 4 . 5)))

  (expect '(1 2 3 (4 . 5))
    (list-utils-alist-flatten '(1 2 3 (4 . 5))))

  (expect '(1 2 3 (4 . 5))
    (list-utils-alist-flatten '(1 (2 3) (4 . 5))))

  (expect '((1 . 2) (3 . 4) (5 . 6) (7 . 8))
    (list-utils-alist-flatten '(((1 . 2) (3 . 4)) ((5 . 6) (7 . 8))))))


(expectations

  (desc "list-utils-insert-before")

  (expect '(1 2 3 four 4 5)
    (list-utils-insert-before '(1 2 3 4 5) 4 'four))

  (expect '(elt 1 2 3 4 5)
    (list-utils-insert-before '(1 2 3 4 5) 1 'elt))

  (expect '(1 2 3 4 elt 5)
    (list-utils-insert-before '(1 2 3 4 5) 5 'elt))

  (expect (error)
    (list-utils-insert-before '(1 2 3 4 5) 6 'elt)))


(expectations

  (desc "list-utils-insert-after")

  (expect '(1 2 3 4 four 5)
    (list-utils-insert-after '(1 2 3 4 5) 4 'four))

  (expect '(1 elt 2 3 4 5)
    (list-utils-insert-after '(1 2 3 4 5) 1 'elt))

  (expect '(1 2 3 4 5 elt)
    (list-utils-insert-after '(1 2 3 4 5) 5 'elt))

  (expect (error)
    (list-utils-insert-after '(1 2 3 4 5) 6 'elt)))


(expectations

  (desc "list-utils-insert-before-pos")

  (expect '(a b c three d e)
    (list-utils-insert-before-pos '(a b c d e) 3 'three))

  (expect '(elt a b c d e)
    (list-utils-insert-before-pos '(a b c d e) 0 'elt))

  (expect '(a b c d elt e)
    (list-utils-insert-before-pos '(a b c d e) 4 'elt))

  (expect (error)
    (list-utils-insert-before-pos '(a b c d e) 5 'elt)))


(expectations

  (desc "list-utils-insert-after-pos")

  (expect '(a b c d three e)
    (list-utils-insert-after-pos '(a b c d e) 3 'three))

  (expect '(a elt b c d e)
    (list-utils-insert-after-pos '(a b c d e) 0 'elt))

  (expect '(a b c d e elt)
    (list-utils-insert-after-pos '(a b c d e) 4 'elt))

  (expect (error)
    (list-utils-insert-after-pos '(a b c d e) 5 'elt)))


(expectations

  (desc "list-utils-plist-reverse")

  (expect '(:four 4 :three 3 :two 2 :one 1)
    (list-utils-plist-reverse '(:one 1 :two 2 :three 3 :four 4)))

  (expect (error)
    (list-utils-plist-reverse '(:one 1 :two 2 :three 3 :four))))


(expectations

  (desc "list-utils-plist-del")

  (expect '(:one 1 :two 2 :three 3 :four 4)
    (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) :six))

  (expect '(:one 1 :two 2 :three 3 :four 4)
    (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) 4))

  (expect '(:one 1 :two 2 :three 3 :four 4)
    (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) 2))

  (expect '(:one 1 :three 3 :four 4)
    (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) :two))

  (expect '(:two 2 :three 3 :four 4)
    (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) :one))

  (expect '(:one 1 :two 2 :three 3)
    (list-utils-plist-del '(:one 1 :two 2 :three 3 :four 4) :four)))

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;

;;; list-utils-test.el ends here
