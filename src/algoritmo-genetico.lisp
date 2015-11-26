(defconstant percentagem-selecao 30)
(defconstant probabilidade-mutacao 10)

(defstruct problemaGen
"ProblemGen
 problema genetico
  - populacao: lista de estadosGen
  - listas-pecas: lista de listas de pecas a usar em cada teste
  - algoritmo: algoritmo a usar quando forem feitos os testes
  - heuristicas: lista de heuristicas que estao a ser avaliadas no problema genetico"
    populacao
    populacao-dim
    listas-pecas
    algoritmo
    heuristicas)

(defstruct estadoGen
"EstadoGen
 estado de uma populacao de um problema genetico
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
    (if (= (list-length lista-fitness) (problemaGen-populacao-dim problemaGen))
        (let ((populacao (problemaGen-populacao problemaGen))
              (lista-selection '())
              (random-max (* 5 (problemaGen-populacao-dim problemaGen))))

            (dotimes (i (problemaGen-populacao-dim problemaGen) lista-selection)

                ;; decide se seleciona o estadoGen ou nao dependendo de uma constante aleatoria e
                ;; uma constante a multiplicar pelo valor de fitness do estadoGen
                (if (< (- 100 percentagem-selecao)
                       (+ (random random-max) (* (nth i lista-fitness) (random random-max))))
                    (setf lista-selection (append lista-selection (list (nth i populacao)))))))))


(defun crossover (problemaGen lista-selection)
"crossover: problemaGen x lista de estadosGen selecionados -> lista com nova populacao"
    (let ((nv-populacao '())
          (lista-selection-dim (list-length lista-selection))
          (n-heuristicas (list-length (problemaGen-heuristicas problemaGen))))  

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
                (setf nv-populacao (append nv-populacao (list (make-estadoGen :constantes nv-constantes))))))))


(defun mutation (populacao)
"mutation: populacao -> populacao com mutacoes"
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
         populacao))


(defun normalize (populacao)
"normalize: populacao -> populacao com estados com constantes normalizadas"
    (map 'list
         #'(lambda (estadoGen)
            (let ((soma-constantes (apply #'+ (estadoGen-constantes estadoGen))))
                (map 'list
                      #'(lambda (constante) (/ constante soma-constantes))
                      (estadoGen-constantes estadoGen))))
         populacao))