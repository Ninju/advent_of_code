(in-package :aoc_2023)

(let ((SLYNK-STICKERS:*BREAK-ON-STICKERS* (list :after)))


(format t "~3&---- START: tree->paths")
(tree->paths
 (lazy-node->list  (build-lazy-spring-tree "a?bc?def"))

 )

  )


(tree->paths (lazy-node->list  (build-lazy-spring-tree "def")))
(tree->paths (lazy-node->list  (build-lazy-spring-tree "def?")))
(tree->paths (lazy-node->list  (build-lazy-spring-tree "abc?def?x")))


 (tree->paths (lazy-node->list  (build-lazy-spring-tree "abc?def")))

(lazy-node-paths *test-tree*)


(mapcar (lambda (xs)
          (format nil "~{~A~}" xs))
        (lazy-node-paths (build-lazy-spring-tree "abc??def")))
(lazy-node->list  (build-lazy-spring-tree "abc??def"))

(let ((SLYNK-STICKERS:*BREAK-ON-STICKERS* (list :after)))
  (->>
   (lazy-node-remove-if-not
    (build-lazy-spring-tree "##?..#")

    (lambda (partial-spring)
      (consume-counts-p partial-spring '(3 1)))

    (lambda (acc cur) (format nil "~A~A" acc cur))
    "")


   (lazy-node->list))


  )
