(defun seleccao (fronteira n)
  "seleccao: lista x inteiro --> lista
Dada uma lista e um inteiro, trunca os elementos da lista de indice igual ou
superior a esse inteiro, excepto se esse inteiro for igual ou superior ao
tamanho da lista, caso em que a deixa intacta."
  (if (>= n (list-length fronteira))
      fronteira
      (subseq fronteira 0 n)))

(defun melhor-no (no-1 no-2)
  "melhor-no: no x no --> no
Dados dois nos, devolve aquele com o estado com maior valor de pontuacao."
  (cond ((null no-1) no-2)
        ((null no-2) no-1)
        (t
         (if (> (estado-pontos (no-estado no-1))
                (estado-pontos (no-estado no-2)))
             no-1
             no-2))))

(defmacro get-time (&body forms)
  "Macro que permite obter o valor do tempo de execucao de um conjunto de formas.
Esta macro e adaptada do site http://cl-cookbook.sourceforge.net/dates_and_times.html."
  (let ((run1   (gensym))
        (run2   (gensym))
        (result (gensym)))
    `(let* ((,run1   (get-internal-run-time))
            (,result (progn ,@forms))
            (,run2   (get-internal-run-time)))
       (float (/ (- ,run2
                    ,run1)
                 internal-time-units-per-second)))))

(defun funcao-avaliacao (estado)
  "funcao-avaliacao: estado --> inteiro
Funcao de avaliacao f(n) = g(n) + h(n). E a funcao de avaliacao utilizada no
procura-best."
  (funcao-avaliacao-parametrizada estado
                                  (list 1 #'custo-buracos)
                                  (list 1 #'custo-oportunidade)
                                  (list 1 #'heuristica-pontuacao-1)))

(defun funcao-avaliacao-parametrizada (estado &rest constantes-heuristicas)
  "funcao-avaliacao-parametrizada: estado x (inteiro heuristica) --> inteiro
Versao mais geral da funcao de avaliacao, sendo uma combinacao linear dos/as
custos/heuristicas que lhes sao passados como argumentos."
  (apply #'+
         (map 'list
              #'(lambda (c-h)
                  (* (car c-h)
                     (funcall (cadr c-h)
                              estado)))
              constantes-heuristicas)))

(defun procura-best (array pecas)
  "procura-best: array x lista pecas --> lista de accoes
Dado um array correspondente a um tabuleiro e uma lista de pecas, devolve uma
lista de accoes que conduzem a um estado solucao, ou nil. Esta e uma versao
modificada do A*. Caracteriza-se por a cada x expansoes efectuar uma seleccao
dos y nos mais promissores, ou seja, com menor valor de f(n). Isto introduz uma
vertente 'greedy' no algoritmo, a favor do desempenho temporal, mas descarta a
optimalidade e a completude do A*. O algoritmo comeca por efectuar a seleccao
com um certo tamanho ajustavel, y, a cada x expansoes de nos. Assim que obtem
uma fronteira nula ou descobre uma solucao, recomeca o algoritmo com x e y igual
ao seu dobro. Caso se tenha descoberto uma solucao, essa e guardada se for a
melhor encontrada ate entao. Apos um limite de tempo configuravel, o algoritmo
devolve a melhor solucao encontrada. Para esta entrega utilizamos um limite de
tempo de 19 segundos por nos ter sido anunciado pelos professores que os testes
sobre esta procura seriam entre 20 e 30 segundos."
  (procura-best-parametrizada array pecas #'funcao-avaliacao 1 8 19))

(defun procura-best-parametrizada (array pecas f seleccao-periodo seleccao-tamanho tempo-limite)
  "procura-best-parametrizada: array x pecas x funcao de avaliacao x inteiro x
inteiro x decimal --> lista de accoes Versao configuravel do procura-best. f
corresponde a funcao de avaliacao; seleccao-periodo corresponde ao numero de
expansoes ate se efectuar a proxima seleccao de nos; seleccao-tamanho
corresponde ao tamanho da seleccao de nos; e o tempo-limite corresponde ao tempo
maximo de execucao do algoritmo."
  (let* ((relogio      0)
         (no-objectivo nil)
         (tabuleiro         (array->tabuleiro array))
         (estado-inicial    (make-estado :tabuleiro tabuleiro
                                         :pecas-por-colocar pecas))
         (fronteira-inicial (list (make-no :estado    estado-inicial
                                           :avaliacao 0
                                           :caminho   nil)))
         (fronteira fronteira-inicial))
    (loop do
      ;; A cada "seleccao-periodo" iteracoes, efectua uma seleccao dos nos mais
      ;; promissores, ou seja, com menor valor de f(n) = g(n) + h(n).
      (dotimes (i seleccao-periodo)
        (incf relogio
              (get-time
               (let ((no (pop fronteira)))
                 (cond
                   ;; Se for encontrado um beco sem saida recomeca com valores
                   ;; de seleccao mais abragentes.
                   ((null no)
                    (setf seleccao-periodo (* 2 seleccao-periodo))
                    (setf seleccao-tamanho (* 2 seleccao-tamanho))
                    (setf fronteira fronteira-inicial))
                   ;; Toma a melhor solucao encontrada ate entao e recomeca com
                   ;; valores de seleccao mais abrangentes.
                   ((solucao (no-estado no))
                    (setf no-objectivo (melhor-no no no-objectivo))

                    (setf seleccao-periodo (* 2 seleccao-periodo))
                    (setf seleccao-tamanho (* 2 seleccao-tamanho))
                    (setf fronteira fronteira-inicial))
                   (t
                    ;; Expande o no e adiciona os nos resultantes a fronteira
                    ;; ordenados pela funcao de avaliacao, f.
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
                                (merge 'list (list novo-no) fronteira #'no<))))))))))
        ;; Se o tempo limite de execucao for ultrapassado, devolve-se a melhor
        ;; solucao encontrada ate entao.
        (if (> relogio tempo-limite)
            (return-from procura-best-parametrizada
              (if no-objectivo
                  (reverse (no-caminho no-objectivo))))))
      ;; Selecciona os nos mais promissores.
      (setf fronteira (seleccao fronteira seleccao-tamanho)))))


