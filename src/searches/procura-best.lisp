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

(defun poda (fronteira n)
  (if (>= n (list-length fronteira))
      fronteira
      (subseq fronteira 0 n)))

(defun funcao-avaliacao (estado)
  (+ (* 00 (funcall #'custo-altura-agregada estado))
     (* 10 (funcall #'custo-oportunidade    estado))
     (* 00 (funcall #'custo-relevo          estado))
     (* 00 (funcall #'custo-buracos         estado))
     (* 10 (funcall #'heuristica-pontuacao  estado))))

(defun procura-best-problema (problema)
  "procura-best-problema: problema --> lista de accoes"
  (let* ((estado-inicial (problema-estado-inicial problema))
         (solucao        (problema-solucao        problema))
         (accoes         (problema-accoes         problema))
         (resultado      (problema-resultado      problema))
         (f              #'funcao-avaliacao)
         (fronteira (list (make-no :estado          estado-inicial
                                   :avaliacao       0
                                   :caminho nil))))
    (loop do
      (dotimes (i 10)
        (let ((no (pop fronteira)))
          (cond ((null no)
                 (return-from procura-best-problema
                   nil))
                ((funcall solucao (no-estado no))
                 (return-from procura-best-problema
                   (reverse (no-caminho no))))
                (t
                 ;; Expande o no e adiciona os nos resultantes a fronteira
                 ;; ordenados pela funcao de avaliacao f(n) = g(n) + h(n).
                 (let* ((estado (no-estado no))
                        (lista-accoes (funcall accoes estado)))
                   (dolist (accao lista-accoes)
                     (let* ((novo-estado (funcall resultado estado accao))
                            (novo-no (make-no :estado novo-estado
                                              :avaliacao (funcall f novo-estado)
                                              :caminho (cons accao
                                                             (no-caminho no)))))
                       ;; Insere o no na fronteira.
                       (setf fronteira
                             (merge 'list
                                    (list novo-no)
                                    fronteira
                                    #'no<)))))))))
      (setf fronteira
            (poda fronteira 1000)))))
