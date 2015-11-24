(defstruct node h g accao antecessor estado)

(defun insere (lista index elemento)
	(cond
		((zerop index) (append (list elemento) lista))
		((>= index (list-length lista)) (append lista (list elemento)))
		(T (progn (push elemento (cdr (nthcdr (1- index) lista))) lista))))

(defun insere-lista-node (lista elemento)
	(dotimes (i (list-length lista))
		(if (<= (+ (node-h elemento) (node-g elemento)) (+ (node-h (nth i lista)) (node-g (nth i lista))))
			(return-from insere-lista-node (insere lista i elemento))))
	(insere lista (list-length lista) elemento))

(defun procura-astar (problema heuristica)
	(let* ((estado-inicial (problema-estado-inicial problema))
		   (open-list (list (make-node :estado estado-inicial :g 0 :h (funcall heuristica estado-inicial)))))

		(loop while (not (funcall (problema-solucao problema) (node-estado (car open-list)))) do
			(let* ((node (car open-list))
				  (estado (node-estado node))
				  (accoes (funcall (problema-accoes problema) estado)))
				(setf open-list (cdr open-list))

				(loop while accoes do
					(let ((nv-estado (funcall (problema-resultado problema) estado (car accoes))))
						(setf open-list
							  (insere-lista-node open-list 
												 (make-node :estado nv-estado
												     		:g (1+ (node-g node))
												  			:h (funcall heuristica nv-estado)
												  			:accao (car accoes)
												  			:antecessor node)))
						(setf accoes (cdr accoes))))))

            (let ((node (make-node :antecessor (car open-list))))
                (reverse (loop while (node-accao (node-antecessor node)) collect 
                    (progn (setf node (node-antecessor node)) (node-accao node)))))))

(load "heuristicas.lisp")