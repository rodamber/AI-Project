(load "tabuleiro.lisp")
(load "estado.lisp")
(load "accao.lisp")
(load "problema.lisp")
(load "utils.lisp")
(load "searches/procura-pp.lisp")
(load "searches/procura-astar.lisp")
(load "searches/procura-best.lisp")
(load "searches/heuristicas.lisp")

;;;-----------------------------------------------------------------------------
;;; Funcoes de teste de execucao das procuras

(defun problema-random (numero-de-pecas)
  (let ((estado (make-estado :pecas-por-colocar (random-pecas numero-de-pecas)
                             :tabuleiro         (cria-tabuleiro))))
    (make-problema :estado-inicial estado)))

(defun pontuacao (procura numero-de-pecas)
  (let* ((problema        (problema-random numero-de-pecas))
         (estado          (problema-estado-inicial problema))
         (lista-de-accoes (time (funcall procura problema)))
         (resultado       (problema-resultado problema)))
    (dolist (accao lista-de-accoes)
      (setf estado
            (funcall resultado
                     estado
                     accao)))
    (estado-pontos estado)))

(defun pontuacao-pp (numero-de-pecas)
  (pontuacao #'procura-pp numero-de-pecas))

(defun pontuacao-A* (heuristica numero-de-pecas)
  (labels
      ((procura (problema)
         (funcall #'procura-A* problema heuristica)))
    (pontuacao #'procura numero-de-pecas)))

(defun pontuacao-best (numero-de-pecas)
  (pontuacao #'procura-best numero-de-pecas))

(defun executa-com-heuristica (funcao-procura heuristica numero-de-pecas)
  (labels
      ((procura (problema)
         (funcall funcao-procura problema heuristica)))
    (executa #'procura numero-de-pecas)))

(defun executa (funcao-procura numero-de-pecas)
  (let* ((problema (problema-random numero-de-pecas))
         (estado   (problema-estado-inicial problema)))
    (executa-jogadas estado
                     (time (funcall funcao-procura
                                    problema)))))

(defun executa-pp (numero-de-pecas)
  (funcall #'executa #'procura-pp numero-de-pecas))

(defun executa-A* (heuristica numero-de-pecas)
  (executa-com-heuristica #'procura-A* heuristica numero-de-pecas))

(defun executa-best (numero-de-pecas)
  (funcall #'executa #'procura-best-problema numero-de-pecas))

;;;-----------------------------------------------------------------------------
;;; Profiling

(defmacro get-time (&body forms)
  (let ((run1 (gensym))
        (run2 (gensym))
        (result (gensym)))
    `(let* ((,run1 (get-internal-run-time))
            (,result (progn ,@forms))
            (,run2 (get-internal-run-time)))
       (float (/ (- ,run2 ,run1) internal-time-units-per-second)))))

;; Doesn't seem to work...
(defun time-mean (form times)
  (apply #'mean
         (loop for i from 1 upto times
               collect (get-time form))))

(defun mean (&rest sequence)
  (if (null sequence)
      nil
      (/ (reduce #'+ sequence) (length sequence))))

