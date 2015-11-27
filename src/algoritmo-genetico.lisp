(defconstant percentagem-selecao 0.5)
(defconstant probabilidade-mutacao 10)
(defconstant tab-aleatorio-decaimento 0.4)
(defconstant tab-aleatorio-prob-inicial 1.0)
(defconstant 25-segundos-clocks (* internal-time-units-per-second 25))

(defstruct problemaGen
"ProblemaGen
 problema genetico
  - populacao: lista de estadosGen
  - listas-pecas: lista de listas de pecas a usar em cada teste
  - listas-tabuleiros: lista de tabuleiros a usar em cada teste
  - algoritmo: algoritmo a usar quando forem feitos os testes
  - heuristicas: lista de heuristicas que estao a ser avaliadas no problema genetico"
    (populacao (list (make-estadoGen :constantes '(1 0 0) :resultado 10) (make-estadoGen :constantes '(0 1 0) :resultado 20) (make-estadoGen :constantes '(0 0 1) :resultado 30)))
    (populacao-dim 3)
    lista-testes
    algoritmo
    (heuristicas '(1 2 3)))

(defstruct teste lista-pecas tabuleiro)

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
           (soma-resultados (apply #'+ 
                (loop for estado in (problemaGen-populacao problemaGen)
                            collect (estadoGen-resultado estado)))))

        (if (> soma-resultados 0)
            (dolist (estado populacao)
                (setf (estadoGen-resultado estado)
                      (float (/ (estadoGen-resultado estado) soma-resultados)))))   
        problemaGen))

(defun selection (problemaGen)
"fitness: problemaGen x lista de probabilidades de selecao -> lista de estadosGen selecionados"
    (let ((populacao (problemaGen-populacao problemaGen))
          (lista-selection '()))

        (loop while (< (list-length lista-selection) (* (problemaGen-populacao-dim problemaGen) percentagem-selecao)) do
            (let ((n-aleatorio (/ (random 100) 100)) (prob-anterior 0))
                (dolist (estado populacao)
                    (if (> (+ prob-anterior (estadoGen-resultado estado)) n-aleatorio)
                        (progn (push estado lista-selection)
                               (setf (problemaGen-populacao problemaGen) (remove estado populacao))
                               (setf problemaGen (fitness problemaGen))
                               (return nil))
                        (incf prob-anterior (estadoGen-resultado estado))))))

        lista-selection))

(defun crossover (problemaGen lista-selection)
"crossover: problemaGen x lista de estadosGen selecionados -> problemaGen com nova populacao"
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
                            (incf (nth (random (list-length constantes)) constantes) 0.1)
                            (decf (nth (random (list-length constantes)) constantes) 0.1))))
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
                          #'(lambda (constante) (float (/ constante soma-constantes)))
                          (estadoGen-constantes estadoGen))))
                estadoGen)
             (problemaGen-populacao problemaGen)))
    problemaGen)

(defun cria-nova-geracao (problemaGen)
  (normalize (mutation (crossover problemaGen (selection (fitness problemaGen))))))

(defun algoritmo-genetico (algoritmo tamanho-populacao numero-testes &rest heuristicas)
    (let* ((n-geracao 0) res-total best
           (n-heuristicas (list-length heuristicas))
           (populacao 
                (cria-lista tamanho-populacao #'(lambda () (make-estadoGen :constantes 
                    (cria-lista n-heuristicas #'(lambda () (random 100)))))))
           (problemaGen (normalize (make-problemaGen :populacao populacao
                                                     :populacao-dim tamanho-populacao
                                                     :algoritmo algoritmo
                                                     :heuristicas heuristicas))))

    (loop do
        (setf (problemaGen-lista-testes problemaGen) (cria-lista numero-testes
            #'(lambda () (make-teste :tabuleiro (cria-tabuleiro-aleatorio tab-aleatorio-prob-inicial tab-aleatorio-decaimento)
                                     :lista-pecas (random-pecas (+ 4 (random 3)))))))
        (setf res-total 0)
        (setf best 0)

        (dolist (estado (problemaGen-populacao problemaGen))        
            (dolist (teste (problemaGen-lista-testes problemaGen))
                (let* ((problema (make-problema
                        :estado-inicial (make-estado :tabuleiro (teste-tabuleiro teste)
                                                     :pecas-por-colocar (teste-lista-pecas teste))))
                      (heuristica #'(lambda (e) (apply #'+ 
                                   (mapcar #'(lambda (c h) (* c (funcall h e))) 
                                           (estadoGen-constantes estado)
                                           (problemaGen-heuristicas problemaGen)))))
                      (tempo-comeco (get-internal-run-time))
                      (pontuacao (funcall algoritmo problema heuristica)))
                    (if (> (- (get-internal-run-time) tempo-comeco) 25-segundos-clocks) (setf pontuacao 0))
                    (incf (estadoGen-resultado estado) pontuacao)))
            (incf res-total (estadoGen-resultado estado))
            (if (< best (estadoGen-resultado estado)) (setf best (estadoGen-resultado estado)))
            (print (format nil " ESTADO || resultado: ~D constantes: ~S" (estadoGen-resultado estado) (estadoGen-constantes estado))))

        (incf n-geracao)
        (setf problemaGen (cria-nova-geracao problemaGen))
        (print (format nil "*************** FIM DA GERACAO ~D | MEDIA: ~,4F | BEST: ~,4F ***************~%"
                    n-geracao 
                    (/ res-total (problemaGen-populacao-dim problemaGen))
                    best))) n-geracao))


(defun media-cons (problemaGen)
    (let* ((populacao (problemaGen-populacao problemaGen))
          (lista (estadoGen-constantes (reduce #'(lambda (e1 e2) (make-estadoGen :constantes (map 'list #'+ (estadoGen-constantes e1) (estadoGen-constantes e2)))) populacao)))
          (soma (reduce #'(lambda (x y) (+ (abs x) (abs y))) lista)))
        (map 'list #'(lambda (n) (/ n soma)) lista)))

(defun cria-lista (n elemento)
    (let ((lista nil))
        (dotimes (i n)
            (push (funcall elemento) lista))
        lista))

(defun procura-demorada (p h)
     (funcall h (problema-estado-inicial p)))

(defun tester ()
    (let ((l '()) (i 100))
        (loop while (>= i 10) do
            (let ((j 30))
                (loop while (>= j 10) do
                    (let (str (sum 0))
                        (dotimes (k 100) (incf sum (algoritmo-genetico #'procura-demorada i j #'heuristica-pontuacao #'heuristica-pecas-por-colocar #'heuristica-relevo #'heuristica-altura-4 #'heuristica-altura-modulo-4)))
                        (setf sum (/ sum 30.0))
                        (setf str (format nil "tamanho-populacao: ~D || numero-testes: ~D || numero-geracoes-media: ~D" i j sum))
                        (print str)
                        (push str l))
                    (decf j 10)))
            (decf i 10)) l))
