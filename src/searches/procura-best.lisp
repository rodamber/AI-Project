(defun procura-best (array pecas)
  "procura-best: array x pecas --> lista de accoes"
  (let* ((tabuleiro (array->tabuleiro array))
         (estado (make-estado :tabuleiro         tabuleiro
                              :pecas-por-colocar pecas))
         (problema (make-problema :estado-inicial estado
                                  :solucao        #'solucao
                                  :accoes         #'accoes
                                  :resultado      #'resultado
                                  :custo-caminho  #'custo-oportunidade)))
    (procura-best-problema problema)))

(defun procura-best-problema (problema)
  "procura-best-problema: problema --> lista de accoes"
  (procura-A* problema)) ; Aqui, nao tera que ser, necessariamente, a procura A*
