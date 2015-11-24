(load "tabuleiro.lisp")
(load "estado.lisp")
(load "accao.lisp")
(load "problema.lisp")
(load "utils.lisp")
(load "searches/procura-pp.lisp")
(load "searches/procura-astar.lisp")
(load "searches/heuristicas.lisp")

(defparameter lista-accoes-1 '((0 . #2A((T T T) (NIL T NIL)))
                               (1 . #2A((T) (T) (T) (T)))
                               (1 . #2A((NIL NIL T) (T T T)))
                               (3 . #2A((NIL T NIL) (T T T)))
                               (6 . #2A((NIL T T) (T T NIL)))))

(setq et1 (make-estado :tabuleiro (cria-tabuleiro) :pecas-por-colocar (random-pecas 30)))

(setq pt1
  (make-problema :estado-inicial et1
                 :solucao #'solucao
                 :accoes #'accoes
                 :resultado #'resultado
                 :custo-caminho #'custo-oportunidade))

(defun teste-com-heuristica (funcao-procura heuristica numero-de-pecas)
  (labels ((procura (problema)
             (funcall funcao-procura problema heuristica)))
    (teste #'procura numero-de-pecas)))

(defun teste (funcao-procura numero-de-pecas)
  (let* ((estado (make-estado :tabuleiro (cria-tabuleiro)
                              :pecas-por-colocar (random-pecas numero-de-pecas)))
         (problema (make-problema :estado-inicial estado
                                  :solucao #'solucao
                                  :accoes #'accoes
                                  :resultado #'resultado
                                  :custo-caminho #'custo-oportunidade)))
    (executa-jogadas estado
                     (time (funcall funcao-procura
                                    problema)))))
