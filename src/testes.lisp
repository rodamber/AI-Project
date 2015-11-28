(load "tabuleiro.lisp")
(load "estado.lisp")
(load "accao.lisp")
(load "problema.lisp")
(load "utils.lisp")
(load "searches/procura-pp.lisp")
(load "searches/procura-astar.lisp")
(load "searches/procura-best.lisp")
(load "searches/heuristicas.lisp")

(defun problema-random (numero-de-pecas)
  (let ((estado (make-estado :pecas-por-colocar (random-pecas numero-de-pecas)
                             :tabuleiro         (cria-tabuleiro))))
    (make-problema :estado-inicial estado)))

;;;-----------------------------------------------------------------------------
;;; Profiling

(defun random-number-from-to (from to)
  (+ from (random (1+ (- to from)))))

(defun executa-accoes (estado accoes)
  (if (null accoes)
      estado
      (executa-accoes (resultado estado
                                 (car accoes))
                      (cdr accoes))))


(defun pontuacao (estado accoes)
 (estado-pontos (executa-accoes estado accoes)))

;; Nao funciona neste momento
;; (defun random-estado (pecas)
;;   (let* ((estado (make-estado :pecas-por-colocar pecas))
;;          (sol    (lambda (estado)
;;                    (and (solucao estado)
;;                         (zerop (estado-pontos estado)))))
;;          (avaliacao (lambda (estado)
;;                       (+ (custo-altura-agregada estado)
;;                          (custo-buracos         estado)
;;                          (heuristica-altura-3  estado))))
;;          (accoes (procura-best-parametrizada array
;;                                              pecas
;;                                              avaliacao
;;                                              10
;;                                              100)))
;;     (executa-accoes estado accoes)))
;; (defun random-array ()
;;   (tabuleiro->array (estado-tabuleiro (random-estado (random-pecas 7)))))

;; (dribble "tabuleiros-pre-computados.lisp")
;; (print (loop for i from 1 upto 100 collect (random-array)))
;; (dribble)

;; (defun heuristica-altura-3 (estado)
;;   "heuristica-altura-3: estado --> numero
;; Recompensa pela altura ser um multiplo de 3"
;;   (let* ((tabuleiro (estado-tabuleiro estado))
;;          (altura-max (apply #'max
;;                             (loop for c
;;                                   from 0 to 9
;;                                   collect (tabuleiro-altura-coluna tabuleiro
;;                                                                    c)))))
;;     (floor (/ altura-max
;;               3))))

(defmacro get-time (&body forms)
  (let ((run1 (gensym))
        (run2 (gensym))
        (result (gensym)))
    `(let* ((,run1 (get-internal-run-time))
            (,result (progn ,@forms))
            (,run2 (get-internal-run-time)))
       (float (/ (- ,run2 ,run1) internal-time-units-per-second)))))

