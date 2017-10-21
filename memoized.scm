(begin 
  ;; read-file produces a list whose elements are the expressions in the file.
  (define (read-file)
    (let ((expr (read)))
      (if (eof-object? expr)
	  '()
	  (cons expr (read-file)))))

  ;; Here we go:  read in the file that defines the graph
  (define data (with-input-from-file "no.dat" read-file))

  ;;; 2D table
  ;; Takes the names of two keys and a table
  ;; return #f if not found else return the value
  (define (lookup key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
      (if subtable
	  (let ((record (assoc key-2 (cdr subtable))))
	    (if record
		(cdr record)
		#f))
	  #f)))

  ;; Insert to table
  (define (insert! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
      (if subtable
	  (let ((record (assoc key-2 (cdr subtable))))
	    (if record
		(set-cdr! record value)
		(set-cdr! subtable
			  (cons (cons key-2 value)
				(cdr subtable)))))
	  (set-cdr! table
		    (cons (list key-1
				(cons key-2 value))
			  (cdr table)))))
    'ok)

  ;; Make table function
  ;; ==>(define test-table (make-table))
  (define (make-table)
    (list '*table*))

  ;;; 1D table
  ;; for keeping tack of min cost
  (define (oned-insert! key value table)
    (let ((record (assoc key (cdr table))))
      (if record
	  (set-cdr! record value)
	  (set-cdr! table
		    (cons (cons key value) (cdr table)))))
    'ok)
  
  (define (oned-lookup key table)
    (let ((record (assoc key (cdr table))))
      (if record
	  (cdr record)
	  #f)))
  ;;;
  ;; (*table* (p2 (end . 11)) (p1 (end . 22) (p2 . 1))
  ;;          (start (p2 . 7) (p1 . 3)))
  ;; children of any node is the cdr of the subtable the key
  ;; ie, start goes to p2 and p1.
  (define (get-children key-1 table)
    (let ((subtable (assoc key-1 (cdr table))))
      (if subtable
	  (cdr subtable)
	  #f)))

  ;; Add all source, target and cost from data to table
  (define (data-to-table data table)
    (cond ((null? data) "DONE")
	  (else
	   (let ((source (car (car data)))
		 (target (cadr (car data)))
		 (cost (caddr (car data))))
	     (insert! source target cost table))
	   (data-to-table (cdr data) table))))
  
  ;; Takes a list of cost.node pairs and return the minimum pair
  ;; ==>(get-min '((1 p1) (2 p2)))
  ;; (1 p1)
  (define (get-min lst)
    (define (iter things minnode)
      (cond ((null? things) minnode)
	    (else
	     (let ((curnode (car things)))
	       (cond ((< (car curnode) (car minnode))
		      (iter (cdr things) curnode))
		     (else
		      (iter (cdr things) minnode))))
	     )
	    ))
    (iter lst '(10000000 holder)))

  ;; Take a list of (cost . node) pairs and combine them
  ;; If the nodes match, only retain one node along with sum of the cost.
  ;; Else sum the cost and combine the nodes into one list of nodes
  ;;
  ;; ==>(combine '((10000 . p1) (1 . p1)))
  ;; (10001 p1)
  ;; ==>(combine '((1000 . p1) (1 . p3)))
  ;; (1001 (p1) p3)
  (define (combine lst)
    (cond ((equal? (cdar lst) (cdadr lst)) 
	   (cons (+ (caar lst) (caadr lst)) ;add the cost
		 (cdar lst)))
	  (else
	   (cons (+ (caar lst) (caadr lst))
		 (append (to-list (cdar lst))
			 (cdadr lst))))))
  ;; Turn non-list into list for append function
  (define (to-list item)        
    (cons item '()))
  
  ;; 2D table for storing nodes and its cost to each child.
  ;; Each table entry will be in the form (b16 (end . 53) (b15 . 30))
  ;; where b16 is the node and key-1 and ((end . 53) (b15 . 30))
  ;; are children of b16 where end and b15 are key-2
  (define ctable (make-table))

  ;; 1D table for storing node and its min cost along with its path to end.
  ;; Each entry will be in the form (<node> <cost> <path>) where node is key.
  ;; ie. (b5 58 (b5 . b19) b19 . end)
  (define minnodes (make-table))


  (define count 0)
  ;; Takes a node and find the min cost of that node
  ;; along with its path. 
  (define (cost node)
    (set! count (+ count 1))
    (define (iter things result)
      (cond ((null? things)      ;end of a the list of children
	     (let ((minnode (get-min result))) ;get the min child
	       (display minnode)
	       (newline)
	       (cond ((pair? (cadr minnode)) ;for cases such as
					     ;(77 (b4 . b16) b16 . end)
		      (oned-insert! (caadr minnode) minnode minnodes))
		     (else                 ;else it's in the form
		                           ;(53 b16 . end)
		      (oned-insert! (cadr minnode) minnode minnodes)))
	       minnode))
	    (else                  ;not end of children list     
	     (let ((child (car things))) ;child is in form (b15 . 30)
	       (let ((target (car child)))
		 (cond ((equal? 'end (car child)) ;child is 'end
                        (iter (cdr things)        ;just lookup
			      (cons
			       (cons (lookup node target ctable)
				     (cons node target))
			       result)))
		       (else                      ;not 'end
			(iter (cdr things)
			      (cons              
			       (combine (append (to-list  ;combine this node
						 (cons    ;with the min child
						  (lookup node target ctable)
						  (cons node target)))
						(to-list (cost target))
						))
			       result))))
		 )))))
    (cond ((oned-lookup node minnodes)    ;check if already calculated 
	   (oned-lookup node minnodes))   ;fetch that cost and path
	  (else
	   (cond ((not (get-children node ctable))
		  (cons 1000000 node))
		 (else
		  (iter (get-children node ctable) '())))))
    )
  
  (data-to-table data ctable)
  (cost 'start)
  )
