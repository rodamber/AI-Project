
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCOES DE CUSTO/ FUNCOES HEURISTICAS

(defun custo-altura-agregada (estado)
  "custo-altura-agregada --> numero
Dado um estado, devolve a soma das alturas de todas as colunas do tabuleiro."
  (let ((tabuleiro (estado-tabuleiro estado))
        (altura-agregada 0))
    (dotimes (coluna 10 altura-agregada)
      (incf altura-agregada
            (tabuleiro-altura-coluna tabuleiro
                                     coluna)))))

(defun custo-buracos (estado)
  "custo-buracos: estado --> numero
Dado um estado, devolve o numero de buracos no tabuleiro. Um buraco no tabuleiro
e uma posicao nao preenchida cuja linha e inferior a linha do topo da sua
coluna."
  (let ((buracos 0)
        (tabuleiro (estado-tabuleiro estado)))
    (dotimes (coluna 10 buracos)
      (incf buracos
            (tabuleiro-coluna-buracos tabuleiro
                                      coluna)))))

(defun custo-pecas-colocadas (estado)
  "custo-pecas-por-colocar: estado --> numero
Dado um estado, devolve o numero de pecas ja colocadas no tabuleiro."
  (list-length (estado-pecas-colocadas estado)))

(defun custo-relevo (estado)
  "custo-relevo: estado --> numero
Dado um estado, devolve uma medida do relevo do tabuleiro. Diz-se que um
tabuleiro tem 'mais relevo' quanto maior forem as diferencas entre as alturas
das suas colunas. Este custo e dado pela soma dos valores absolutos das
diferencas de altura entre todas as colunas adjacentes."
  (let ((relevo 0)
        (tabuleiro (estado-tabuleiro estado)))
    (loop for coluna from 1 upto 9 do
      (let ((a1 (tabuleiro-altura-coluna tabuleiro coluna))
            (a2 (tabuleiro-altura-coluna tabuleiro (1- coluna))))
        (incf relevo (abs (- a1 a2)))))
    relevo))

(defun heuristica-pontuacao-1 (estado)
  "heuristica-pontuacao: estado --> numero
Dado um estado, devolve a diferenca entre a pontuacao maxima que e possivel
obter colocando as pecas por colocar, no estado inicial, num tabuleiro vazio e a
pontuacao obtida. A pontuacao maxima que e possivel obter num tabuleiro vazio e
800 vezes o numero de blocos de 4 linhas que e possivel construir com as pecas
dadas. Como cada peca (tetromino) tem 4 blocos constituintes (minos), precisamos
de um minimo de 10 pecas para fazer 800 pontos. Assim, um limite mais apertado
para a pontuacao maxima e 80 vezes o numero de pecas por colocar no estado
inicial."
  (let* ((numero-pecas (+ (list-length (estado-pecas-colocadas estado))
                          (list-length (estado-pecas-por-colocar estado))))
         (pontuacao-maxima (* 80 numero-pecas)))
    (- pontuacao-maxima (estado-pontos estado))))

(defun heuristica-pontuacao-2 (estado)
  "heuristica-pontuacao: estado --> numero
Dado um estado, devolve a diferenca entre a pontuacao maxima que e possivel obter
(com as pecas por colocar inicialmente), supondo que cada peca rende a pontuacao
maxima possivel, e o pontos ja obtidos ate entao."
  (- (+ (pontos-maximos (estado-pecas-por-colocar estado))
        (pontos-maximos (estado-pecas-colocadas   estado)))
     (estado-pontos estado)))

(defun heuristica-altura (estado)
  "heuristica-altura: estado --> numero
Dado um estado, devolve a altura da coluna mais alta do tabuleiro."
  (let* ((tabuleiro (estado-tabuleiro estado))
         (alturas-colunas (loop for c from 0 to 9 collect (tabuleiro-altura-coluna tabuleiro c))))
    (apply #'max alturas-colunas)))

