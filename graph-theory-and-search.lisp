;;; exploration of both Land of Lisp code for drawing graphs and ANSI
;;; Common Lisp code for doing breadth-first-search of graphs
;;;
;;; In order to use CLISP's system extensions, it is evidently crucial
;;; to start emacs from the terminal! Failure to do so could have been
;;; the culprit in receiving a 127 error code


;; a given minimal network
(setf min '((a b c) (b c) (c d)))
(setf min2 '((a b c) (b c e) (c d f)))

(my-graph->dot min) 			;digraph{A->B;A->C;B->C;C->D;}"}"

(my-graph->png "min2" min2)		;creates two files in the
					;directory Emacs started

;; PG's code from book examples

;; example use: find the shortest path from 'a' to 'd' given a network
;; 'min'

(shortest-path 'a 'd min)

;; (trace bfs)

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

;; how to create a file for use with graphviz, using a filename FNAME
;; and a THUNK argument that will produce the output piped into the
;; file

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   (concatenate 'string fname ".dot")
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  ;; CLISP has a shell library that we use here
  (ext:shell (concatenate 'string "dot -Tpng -O " fname ".dot")))

;;; create the DOT file given the nodes and edges. primarily serves to
;;; wrap a call to the dot->png function


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
