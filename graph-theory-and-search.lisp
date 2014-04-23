;;; exploration of both Land of Lisp code for drawing graphs and ANSI
;;; Common Lisp code for doing breadth-first-search of graphs

;; a given minimal network
(setf min '((a b c) (b c) (c d)))
(setf min2 '((a b c) (b c e) (c d f)))

;; example use: find the shortest path from 'a' to 'd' given a network
;; 'min'

(shortest-path 'a 'd min)
(shortest-path 'a 'f min2) 		;(A C F)

(my-graph->dot min) 			;digraph{A->B;A->C;B->C;C->D;}"}"
(my-graph->png "min2" min2)		;creates two files in home directory


;;;; evaluate region from here down
(setf min '((a b c) (b c) (c d)))
(setf min2 '((a b c) (b c e) (c d f)))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

;; PG's code from book examples

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net))
		   net))))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;;; Barski's 

;; take care of proper labeling for DOT format
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

;; properly format nodes for DOT format

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

;; properly format edges between nodes for DOT format
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->") 		;DOT uses 'arrow' for an edge
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

;; take a directed graph of nodes and edges, process them and wrap
;; them in the proper DOT format. use of PRINC throughout ensures that
;; output will be a string to a stream that needs to be captured

(defun dgraph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


;; how to create a file for use with graphviz, using a filename FNAME
;; and a THUNK argument that will produce the output piped into the
;; file

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   (concatenate 'string fname ".dot") :direction :output :if-exists :supersede)
    (funcall thunk))
  ;; CLISP has a shell library that we use here
  (ext:shell (concatenate 'string "dot -Tpng -O " fname ".dot")))

;;; create the DOT file given the nodes and edges. primarily serves to
;;; wrap a call to the dot->png function

(defun dgraph->png (fname nodes edges)
  (dot->png fname
	    ;; anonymous function as the THUNK
            (lambda ()
              (dgraph->dot nodes edges))))

;;; my code for building up the needed node data for sending off
;;; recursively chew through a NODE-LIST

;; what structure is the node-list in? See PG network above?
(defun build-nodes (node-list)
  (let* ((head (car node-list))
	 (tail (cdr node-list)))
    (labels ((rec (lst)
	       (cond ((null lst) ())
		     (t (cons (list head (car lst))
			      (rec (cdr lst)))))))
      (rec tail))))

;;; print a pair of nodes with an arrow for DOT format
(defun convert (cns)
  (princ (car cns))
  (princ "->")
  (princ (cadr cns))
  (princ ";"))

;;; my version of the Barski function above. Because it is not taking
;;; NODES-EDGES pairs, but instead NODE-LST a la Graham, I needed to
;;; build my own little processing in between the DOT digraph
;;; directive

(defun my-graph->dot (node-lst)
  (princ "digraph{")
  (mapcar #'convert (mapcan #'build-nodes node-lst))
  (princ "}"))

;;; my version of the master function for creating png files of
;;; networks. top-level. see above.

(defun my-graph->png (fname node-lst)
  "produces graphs given a filename and list of nodes"
  (dot->png fname
	    (lambda ()
	      (my-graph->dot node-lst))))
