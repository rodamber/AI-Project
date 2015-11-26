(defconstant percentagem-selecao 30)
(defconstant probabilidade-mutacao 10)

(defstruct problemaGen
"ProblemGen
 problema genetico
  - populacao: lista de estadosGen
  - listas-pecas: lista de listas de pecas a usar em cada teste
  - listas-tabuleiros: lista de tabuleiros a usar em cada teste
  - algoritmo: algoritmo a usar quando forem feitos os testes
  - heuristicas: lista de heuristicas que estao a ser avaliadas no problema genetico"
    (populacao (list (make-estadoGen :constantes '(1 0 0) :resultado 10) (make-estadoGen :constantes '(0 1 0) :resultado 20) (make-estadoGen :constantes '(0 0 1) :resultado 30)))
    (populacao-dim 3)
    listas-pecas
    listas-tabuleiros
    algoritmo
    (heuristicas '(1 2 3)))

(defstruct estadoGen
"EstadoGen
 estado de uma populacao de estados de um problema genetico
  - constantes: lista com as constantes relacionadas com cada heuristica
  - resultado: soma das pontuacoes apos se correr os testes usando o algoritmo e a heuristica 
        especificadas no problemaGen"
    constantes
    (resultado 0))



(defun fitness (problemaGen)
"fitness: problemaGen -> lista de probabilidades de selecao"
    (let* ((populacao (problemaGen-populacao problemaGen))
           (lista-fitness
                (loop for i from 0 to (1- (problemaGen-populacao-dim problemaGen))
                    collect (estadoGen-resultado (nth i populacao))))
           (soma-resultados (apply #'+ lista-fitness)))

        (if (> soma-resultados 0)
            (map 'list #'(lambda (n) (/ n soma-resultados)) lista-fitness))))


(defun selection (problemaGen lista-fitness)
"fitness: problemaGen x lista de probabilidades de selecao -> lista de estadosGen selecionados"
    (let ((populacao (problemaGen-populacao problemaGen))
          (lista-selection '()))

        (loop while (null lista-selection) do
            (dotimes (i (problemaGen-populacao-dim problemaGen) lista-selection)

                ;; decide se seleciona o estadoGen ou nao dependendo de uma constante aleatoria e
                ;; uma constante a multiplicar pelo valor de fitness do estadoGen
                (if (< (- 100 percentagem-selecao) (* (nth i lista-fitness) (random 100)))
                    (setf lista-selection (append lista-selection (list (nth i populacao)))))))
        lista-selection))


(defun crossover (problemaGen lista-selection)
"crossover: problemaGen x lista de estadosGen selecionados -> lista com nova populacao"
    (let ((nv-populacao '())
          (lista-selection-dim (list-length lista-selection))
          (n-heuristicas (list-length (problemaGen-heuristicas problemaGen))))  

        (setf (problemaGen-populacao problemaGen)
            (dotimes (i (problemaGen-populacao-dim problemaGen) nv-populacao)

                ;; seleciona dois pais para fazer o cruzamento para o novo filho
                (let ((pai1 (estadoGen-constantes (nth (random lista-selection-dim) lista-selection)))
                      (pai2 (estadoGen-constantes (nth (random lista-selection-dim) lista-selection)))
                      (nv-constantes '()))

                    ;; seleciona aleatoriamente as constantes de ambos os pais
                    (dotimes (j n-heuristicas)
                        (setf nv-constantes
                              (append nv-constantes 
                                      (list (nth j (if (> (random 2) 0) pai1 pai2))))))

                    ;; cria o novo estadoGen e adiciona a lista da nova populacao
                    (setf nv-populacao
                        (append nv-populacao
                            (list (make-estadoGen :constantes nv-constantes)))))))
        problemaGen))


(defun mutation (problemaGen)
"mutation: problemaGen -> problemaGen com populacao com mutacoes"
    (setf (problemaGen-populacao problemaGen)
        (map 'list
             #'(lambda (estadoGen)

                ;; decide se vai alterar o estado ou nao
                (if (< (random 100) probabilidade-mutacao)

                    ;; decide se vai incrementar ou decrementar uma constante aleatoria em 0.5
                    (if (> (random 2) 0)
                        (let ((constantes (estadoGen-constantes estadoGen)))
                            (incf (nth (random (list-length constantes)) constantes) 0.5)
                            (decf (nth (random (list-length constantes)) constantes) 0.5))))
                estadoGen)
             (problemaGen-populacao problemaGen)))
    problemaGen)


(defun normalize (problemaGen)
"normalize: problemaGen -> problemaGen com populacao com constantes normalizadas"
    (setf (problemaGen-populacao problemaGen)
        (map 'list
             #'(lambda (estadoGen)
                (let ((soma-constantes (reduce #'(lambda (x y) (+ (abs x) (abs y))) (estadoGen-constantes estadoGen))))
                    (if (zerop soma-constantes) (setf soma-constantes 1))
                    (setf (estadoGen-constantes estadoGen) (map 'list
                          #'(lambda (constante) (/ constante soma-constantes))
                          (estadoGen-constantes estadoGen))))
                estadoGen)
             (problemaGen-populacao problemaGen)))
    problemaGen)


(defun cria-nova-geracao (problemaGen)
  (normalize (mutation (crossover problemaGen (selection problemaGen (fitness problemaGen))))))

(defun algoritmo-genetico (algoritmo tamanho-populacao numero-testes numero-pecas &rest heuristicas)
    (let* ((n-heuristicas (list-length heuristicas))
           (populacao 
                (cria-lista tamanho-populacao #'(lambda () (make-estadoGen :constantes 
                    (cria-lista n-heuristicas #'(lambda () (random 100)))))))
           (lista-pecas (cria-lista numero-testes #'(lambda () (random-pecas (random numero-pecas)))))
           (problemaGen (normalize (make-problemaGen :populacao populacao
                                                     :populacao-dim tamanho-populacao
                                                     :lista-pecas lista-pecas
                                                     :algoritmo algoritmo
                                                     :heuristicas heuristicas))))))



(defun cria-lista (n elemento)
    (let ((lista-constantes nil))
        (dotimes (i n)
            (push (funcall elemento) lista-constantes))
        lista-pecas))