(defun poda (fronteira n)
  (if (>= n (list-length fronteira))
      fronteira
      (subseq fronteira 0 n)))

(defun funcao-avaliacao-parametrizada (estado a b c)
  (+ (* a (custo-oportunidade   estado))
     (* b (heuristica-altura    estado))
     (* c (heuristica-pontuacao estado))))

(defun funcao-avaliacao (estado)
  (funcao-avaliacao-parametrizada estado 1 0 1))

(defun procura-best (array pecas)
  "procura-best-problema: array x pecas --> lista de accoes"
  (procura-best-parametrizada array
                              pecas
                              #'funcao-avaliacao
                              10
                              100))

(defun procura-best-fixa (array pecas f)
  (procura-best-parametrizada array pecas f 10 10))

(defun procura-best-parametrizada (array pecas f poda-periodo poda-tamanho)
  (let* ((tabuleiro      (array->tabuleiro array))
         (estado-inicial (make-estado :tabuleiro tabuleiro
                                      :pecas-por-colocar pecas))
         (fronteira      (list (make-no :estado    estado-inicial
                                        :avaliacao 0
                                        :caminho   nil))))
    (loop do
      (dotimes (i poda-periodo)
        (let ((no (pop fronteira)))
          (cond
            ((null no)
             (return-from procura-best-parametrizada
               nil))
            ((solucao (no-estado no))
             (return-from procura-best-parametrizada
               (reverse (no-caminho no))))
            (t
             ;; Expande o no e adiciona os nos resultantes a fronteira
             ;; ordenados pela funcao de avaliacao f(n) = g(n) + h(n).
             (let* ((estado       (no-estado no))
                    (lista-accoes (accoes estado)))
               (dolist (accao lista-accoes)
                 (let* ((novo-estado (resultado estado accao))
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
            (poda fronteira
                  poda-tamanho)))))
