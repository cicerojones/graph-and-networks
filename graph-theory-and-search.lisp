
;;; unrelated exploration of both Land of Lisp code for drawing
;;; graphs and ANSI Common Lisp code for doing breadth-first-search of
;;; graphs


;; a given minimal network
(setf min '((a b c) (b c) (c d)))

;; example use: find the shortest path from 'a' to 'd' given a network
;; 'min'

(shortest-path 'a 'd min)

;; examples of graph structures from Land of Lisp

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                             ))
                               (garden (you are you.))
                               (attic (you are in the))))

(defparameter *wizard-edges* '((living-room (garden west door)  
                                (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

;; needed for conversion note, uses the COMPLEMENT technique to find
;; non-alphanumeric characters. substitute-if replaces any character
;; that satisfies the predicate with its first argument, in this case
;; the escaped underscore character.

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

;; Barski's code
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun dgraph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;; how to create a file for use with graphviz
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   (concatenate 'string fname ".dot") :direction :output :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname ".dot")))

(defun dgraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (dgraph->dot nodes edges))))

;; my code for building up the needed node data for sending off
(defun build-nodes (node-list)
  (let* ((head (car node-list))
	 (tail (cdr node-list)))
    (labels ((rec (lst)
	       (cond ((null lst) ())
		     (t (cons (list head (car lst))
			      (rec (cdr lst)))))))
      (rec tail))))

(defun convert (cns)
  (princ (car cns))
  (princ "->")
  (princ (cadr cns))
  (princ ";"))

(defun my-graph->dot (node-lst)
  (princ "digraph{")
  (mapcar #'convert (mapcan #'build-nodes node-lst))
  (princ "}"))

;; the master function for creating png files of graphs
(defun my-graph->png (fname node-lst)
  "produces graphs given a filename and list of nodes"
  (dot->png fname
	    (lambda ()
	      (my-graph->dot node-lst))))

;(defun run ()
;  (ugraph->png "wizard" *nodes* *edges*))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))
  
(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))



(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

;; simple with-open-file examples


(defun write-junk (fname str-message)
  "creates .txt files with a given fname, containing messages passed in as strings"
	   (with-open-file (*standard-output*
			    (concatenate 'string fname ".txt")
			    :direction :output
			    :if-exists :supersede)
	     (princ str-message)))


;; version of function used to create files. here explicitly called
;; using PRINC and an existing digraph structure

(defun write-junk-digraphs (fname)
  "creates a single, simple static messages for parsing by graph-to-png functions"
  (with-open-file (*standard-output*
		   (concatenate 'string fname ".dot") 
		   :direction :output 
		   :if-exists :supersede)
    (princ "digraph {a->b; a->c;}")))


;; (write-junk-digraphs "testes5")

'((a b c) (b c) (c d))

(let ((ex '(a b)))
  (labels 
      ((convert (cns)
	 (princ (car cns))
	 (princ "->")
	 (princ (cadr cns))
	 (princ ";")))
    (convert ex)))
