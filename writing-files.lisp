;;; simple with-open-file examples. useful for saving output to a file


(defun write-junk (fname str-message)
  "creates .txt files with a given fname, containing messages passed in as strings"
  (with-open-file (*standard-output*
		   (concatenate 'string fname ".txt")
		   :direction :output
		   :if-exists :supersede)
    (princ str-message)))


;;; model for .txt version above. not useful as is because of built-in
;;; string message

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
