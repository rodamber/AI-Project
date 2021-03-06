;; TIPO PROBLEMA

(defstruct problema
  "Estrutura do problema do jogo
- estado-inicial: contem o estado inicial do problema de procura;
- solucao: funcao que recebe um estado e devolve T se o estado
    for uma solucao para o problema de procura, e nil caso contrario;
- accoes: funcao que recebe um estado e devolve uma lista com todas
    as accoes que sao possiveis fazer nesse estado;
- resultado: funcao que dado um estado e uma accao devolve o estado
    sucessor que resulta de executar a accao recebida no estado recebido;
- custo-caminho: funcao que dado um estado devolve o custo do caminho
    desde o estado inicial ate esse estado."
  (estado-inicial (make-estado))
  (solucao #'solucao)
  (accoes #'accoes)
  (resultado #'resultado)
  (custo-caminho #'custo-oportunidade))

(defun solucao (estado)
  "solucao: estado -> logico
Esta funcao recebe um estado, e devolve o valor logico verdade se o estado
recebido corresponder a uma solucao, e falso contrario. Um estado do jogo
Tetris e considerado solucao se o topo do tabuleiro nao estiver preenchido e
se ja nao existem pecas por colocar, ou seja, todas as pecas foram colocadas
com sucesso."
  (let* ((tabuleiro  (estado-tabuleiro            estado))
         (pecas      (estado-pecas-por-colocar    estado))
         (preenchido (tabuleiro-topo-preenchido-p tabuleiro)))
    (and (not preenchido)
         (null pecas))))

(defun accoes (estado)
  "accoes: estado -> lista de accoes
Esta funcao recebe um estado e devolve uma lista de accoes correspondendo a
todas as accoes validas que podem ser feitas com a proxima peca a ser
colocada. Uma accao e considerada valida mesmo que faca o jogador perder o
jogo."
  (if (estado-final-p estado)
      nil
      (let ((lista-accoes nil)
            (numero-de-colunas 10))
        (labels ((accoes-validas (peca)
                   (let* ((largura (array-dimension peca
                                                    1))
                          (colunas-validas (- (1+ numero-de-colunas)
                                              largura)))
                     (dotimes (i colunas-validas)
                       (setf lista-accoes
                             (append lista-accoes
                                     (list (cria-accao i
                                                       peca))))))))
          (case (first (estado-pecas-por-colocar estado))
            (i (map 'list #'accoes-validas (list peca-i0 peca-i1)))
            (l (map 'list #'accoes-validas (list peca-l0 peca-l1 peca-l2 peca-l3)))
            (o (map 'list #'accoes-validas (list peca-o0)))
            (s (map 'list #'accoes-validas (list peca-s0 peca-s1)))
            (z (map 'list #'accoes-validas (list peca-z0 peca-z1)))
            (j (map 'list #'accoes-validas (list peca-j0 peca-j1 peca-j2 peca-j3)))
            (t (map 'list #'accoes-validas (list peca-t0 peca-t1 peca-t2 peca-t3))))
          lista-accoes))))

(defun resultado (estado accao)
  "resultado: estado x accao -> estado
Esta funcao recebe um estado e uma accao, e devolve um novo estado que
resulta de aplicar a accao recebida no estado original."
  (let* ((novo-estado (copia-estado estado))
         (letra       (first (estado-pecas-por-colocar novo-estado)))
         (tabuleiro   (estado-tabuleiro novo-estado))

         (novas-pecas-por-colocar (rest (estado-pecas-por-colocar novo-estado)))
         (novas-pecas-colocadas   (cons letra (estado-pecas-colocadas novo-estado))))

    (progn (setf (estado-pecas-por-colocar novo-estado)
                 novas-pecas-por-colocar)
           (setf (estado-pecas-colocadas novo-estado)
                 novas-pecas-colocadas)
           (tabuleiro-executa-accao! tabuleiro accao)
           (when (not (tabuleiro-topo-preenchido-p tabuleiro))
             (let ((n-linhas-completas (tabuleiro-remove-linhas-completas! tabuleiro)))
               (case n-linhas-completas
                 (1 (incf (estado-pontos novo-estado) 100))
                 (2 (incf (estado-pontos novo-estado) 300))
                 (3 (incf (estado-pontos novo-estado) 500))
                 (4 (incf (estado-pontos novo-estado) 800)))))
           novo-estado)))

(defun qualidade (estado)
  "qualidade: estado -> inteiro
Esta funcao recebe um estado e retorna um valor de qualidade que corresponde ao
valor negativo dos pontos ganhos ate ao momento."
  (* -1 (estado-pontos estado)))

(defun custo-oportunidade (estado)
  "custo-oportunidade: estado -> inteiro
  Esta funcao, dado um estado, devolve o custo de oportunidade de todas as accoes
realizadas ate ao momento, assumindo que e sempre possivel fazer o maximo de pontos
por cada peca colocada"
  (- (pontos-maximos (estado-pecas-colocadas estado))
     (estado-pontos estado)))

(defun pontos-maximos (lis)
  "pontos-maximos: lista de pecas --> inteiro
Recebe uma lista pecas e devolve a pontuacao maxima que se pode obter com essas pecas,
assumindo que e sempre possivel fazer o maximo de pontos por cada peca colocada."
  (let ((f (first lis))
        (r (rest  lis)))
    (cond ((equal f 'i)      (+ (pontos-maximos r) 800))
          ((or (equal f 'j)
               (equal f 'l)) (+ (pontos-maximos r) 500))
          ((or (equal f 's)
               (equal f 'z)
               (equal f 't)
               (equal f 'o)) (+ (pontos-maximos r) 300))
          (t 0))))
