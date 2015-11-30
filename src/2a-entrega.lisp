;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEIC-A
;; Grupo 48
;; 78941 - Micael Batista
;; 78942 - Rodrigo Bernardo
;; 78960 - Francisco Besteiro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIPO TABULEIRO
;;; Este e representado atraves de um array 2D, para ter acesso O(1) a cada
;;; posicao do tabuleiro.

(defun cria-tabuleiro ()
  "cria-tabuleiro: {} --> tabuleiro
Devolve um novo tabuleiro vazio."
  (make-array '(18 10)))

(defun copia-tabuleiro (tabuleiro)
  "copia-tabuleiro: tabuleiro --> tabuleiro
Copia e devolve um tabuleiro igual ao recebido como argumento. Este e uma
entidade diferente da primeira."
  (copia-array2D tabuleiro))

(defun copia-array2D (array)
  "copia-array2D: array --> array
Copia e devolve um array igual ao recebido como argumento. Este e uma
entidade diferente da primeira."
  (let* ((dimensoes (array-dimensions array))
         (novo-array (make-array dimensoes)))
    (dotimes (i (car dimensoes) novo-array)
      (dotimes (j (cadr dimensoes))
        (setf (aref novo-array i j)
              (aref array i j))))))

(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
  "tabuleiro-preenchido-p: tabuleiro x inteiro x inteiro --> logico
Recebe um tabuleiro, linha e coluna, e devolve T se essa posicao estiver
preenchida, e NIL caso contrario."
  (aref tabuleiro linha coluna))

(defun tabuleiro-altura-coluna (tabuleiro coluna)
  "tabuleiro-altura-coluna: tabuleiro x inteiro --> inteiro
Recebe um tabuleiro e coluna, e devolve a posicao mais alta dessa coluna que
esteja preenchida."
  (let ((numero-de-linhas 18))
    (loop for linha from (1- numero-de-linhas) downto 0
          do (when (tabuleiro-preenchido-p tabuleiro linha coluna)
               (return-from tabuleiro-altura-coluna (1+ linha))))
    0))

(defun tabuleiro-linha-completa-p (tabuleiro linha)
  "tabuleiro-linha-completa-p: tabuleiro x inteiro --> logico
Recebe um tabuleiro e linha, e devolve T se todas as posicoes dessa linha
estiverem preenchidas, e NIL caso contrario."
  (let ((numero-de-colunas 10))
    (dotimes (coluna numero-de-colunas t)
      (when (not (tabuleiro-preenchido-p tabuleiro linha coluna))
        (return nil)))))

(defun tabuleiro-preenche! (tabuleiro linha coluna)
  "tabuleiro-preenche!: tabuleiro x inteiro x inteiro --> {}
Recebe um tabuleiro, linha e coluna, e altera o tabuleiro para na posicao
correspondente a linha e coluna passar a estar preenchido. Se o numero de linha
e de coluna recebidos nao forem validos (i.e. nao estiverem entre 0 e 17, e 0 e
9), nada e feito. O valor devolvido por esta funcao nao esta definido"
  (when (and (>= linha  0)
             (>= coluna 0)
             (< linha  18)
             (< coluna 10))
    (setf (aref tabuleiro linha coluna) t)))

(defun tabuleiro-remove-linha! (tabuleiro linha)
  "tabuleiro-remove-linha!: tabuleiro x inteiro --> {}
Recebe um tabuleiro e linha, e altera o tabuleiro recebido removendo essa linha
do tabuleiro, fazendo com que as linhas por cima da linha removida descam uma
linha. As linhas que estao por baixo da linha removida nao podem ser alteradas.
O valor devolvido por esta funcao nao esta definido."
  (let ((numero-de-linhas 18))
    (loop for i from linha to (- numero-de-linhas 2) do
      ;; Trocam-se as linhas de modo a que a linha a ser removida se encontre no
      ;; topo do tabuleiro no final do ciclo e que as suas superiores descam uma
      ;; linha. Se a linha for a do topo, nao ha necessidade de fazer trocas.
      (tabuleiro-troca-linhas! tabuleiro i (1+ i)))
    (dotimes (i 10)
      ;; No final, "remove-se" a linha do topo.
      (setf (aref tabuleiro (1- numero-de-linhas) i) nil))))

(defun tabuleiro-troca-linhas! (tabuleiro linha-1 linha-2)
  "tabuleiro-troca-linhas!: tabuleiro x inteiro x inteiro --> tabuleiro
Recebe um tabuleiro e duas linhas e troca os conteudos dessas linhas."
  (dotimes (i 10)
    (let ((x (aref tabuleiro linha-1 i))
          (y (aref tabuleiro linha-2 i)))
      (setf (aref tabuleiro linha-1 i) y)
      (setf (aref tabuleiro linha-2 i) x))))

(defun tabuleiro-remove-linhas-completas! (tabuleiro)
  "tabuleiro-remove-linhas-completas!: tabuleiro --> inteiro
Remove todas as linhas do tabuleiro que estejam completas. Devolve o numero de
linhas removidas."
  (let ((numero-de-linhas 18)
        (linhas-removidas 0))
    (loop for linha from (1- numero-de-linhas) downto 0 do
      (when (tabuleiro-linha-completa-p tabuleiro linha)
        (progn (incf linhas-removidas)
               (tabuleiro-remove-linha! tabuleiro linha))))
    linhas-removidas))

(defun tabuleiro-topo-preenchido-p (tabuleiro)
  "tabuleiro-topo-preenchido-p: tabuleiro --> logico
Recebe um tabuleiro e devolve T se existir alguma posicao na linha do topo do
tabuleiro que esteja preenchida, e NIL caso contrario."
  (let ((numero-de-linhas  18)
        (numero-de-colunas 10))
    (dotimes (coluna numero-de-colunas nil)
      (when (tabuleiro-preenchido-p tabuleiro (1- numero-de-linhas) coluna)
        (return t)))))

(defun tabuleiros-iguais-p (tab1 tab2)
  "tabuleiros-iguais-p: tabuleiro x tabuleiro --> logico
Devolve T se os dois tabuleiros forem iguais (i.e. tiverem o mesmo conteudo), e
NIL caso contrario"
  (equalp tab1 tab2))

(defun tabuleiro->array (tabuleiro)
  "tabuleiro->array: tabuleiro --> array
Recebe um tabuleiro e devolve um novo array com 18 linhas e 10 colunas, que para
cada linha e coluna devera conter o valor logico correspondente a cada posicao
do tabuleiro. Nenhuma alteracao no tabuleiro tem repercussao no array devolvido
e vice-versa."
  (copia-tabuleiro tabuleiro))

(defun array->tabuleiro (array)
  "array->tabuleiro: array --> tabuleiro
Recebe um array com 18 linhas e 10 colunas cujas posicoes tem o valor logico T
ou NIL, e constroi um novo tabuleiro com o conteudo do array recebido. Nenhuma
alteracao no array original tem repercussao no tabuleiro devolvido e
vice-versa."
  (copia-array2D array))

(defun tabuleiro-executa-accao! (tabuleiro accao)
  "tabuleiro-executa-accao!: tabuleiro x accao --> tabuleiro
Recebe um tabuleiro e devolve um novo tabuleiro com a peca colocada na coluna
correspondente de acordo com o parametro accao. Esta funcao nao verifica a
validade da accao, ou seja, nao verifica se a peca sera colocada for dos limites
laterais do tabuleiro."
  (let* ((coluna (accao-coluna accao))
         (peca   (accao-peca   accao))

         (largura-peca (array-dimension peca 1))

         ;; Lista com as alturas das colunas abrangidas pela peca. Por exemplo,
         ;; uma peca com largura 3 a ser colocada na coluna 2 abrange as colunas
         ;; 2, 3 e 4 do tabuleiro.
         (alturas-colunas (loop for c from coluna to (1- (+ coluna largura-peca))
                                collect (tabuleiro-altura-coluna tabuleiro c)))
         (linha (apply #'max
                       (mapcar #'-
                               alturas-colunas
                               (peca-alturas-buracos peca)))))
    (tabuleiro-coloca-peca! tabuleiro peca linha coluna)))

(defun tabuleiro-coloca-peca! (tabuleiro peca linha coluna)
  "tabuleiro-coloca-peca!: tabuleiro x peca x inteiro x inteiro --> tabuleiro
Recebe um tabuleiro, a peca a colocar no tabuleiro e a linha e a coluna onde
posicionar o elemento inferior esquerdo da peca. Esta funcao nao verifica se e
valido colocar a peca na posicao pretendida."
  (let ((altura-peca  (array-dimension peca 0))
        (largura-peca (array-dimension peca 1)))
    (dotimes (i altura-peca tabuleiro)
      (dotimes (j largura-peca)
        (when (aref peca i j)
          (tabuleiro-preenche! tabuleiro
                               (+ linha  i)
                               (+ coluna j)))))))

(defun tabuleiro-coluna-buracos (tabuleiro coluna)
  "tabuleiro-coluna-buracos: tabuleiro x inteiro --> inteiro
Recebe um tabuleiro e um inteiro que representa a coluna escolhida e devolve o
numero de posicoes nao preenchidas abaixo da altura dessa coluna."
  (let ((altura (tabuleiro-altura-coluna tabuleiro coluna))
        (buracos 0))
    (dotimes (linha altura buracos)
      (when (not (tabuleiro-preenchido-p tabuleiro linha coluna))
        (incf buracos)))))

(defun max-indice (lista)
  "max-indice: lista --> inteiro
Recebe uma lista e devolve o indice do elemento maximo dessa lista."
  (let ((max    (apply #'max lista))
        (indice 0))
    (dolist (x lista)
      (if (= x max)
          (return indice)
          (incf indice)))))

(defun peca-alturas-buracos (peca)
  "altura-buracos: peca --> lista de inteiros positivos
Dada uma peca esta funcao devolve uma lista onde o elemento de indice i da lista
corresponde a altura da posicao preenchida na coluna i da peca com a linha mais
baixa."
  (let* ((largura-peca (array-dimension peca 1))
         (alturas nil))
    (dotimes (coluna largura-peca (reverse alturas))
      (let ((linha 0))
        (progn (loop while (not (aref peca linha coluna)) do (incf linha))
               (setf alturas (cons linha alturas)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIPO ESTADO

(defstruct estado
  "Estrutura do estado do jogo
- pontos: o numero de pontos conseguidos ate ao momento no jogo.
- pecas-por-colocar: uma lista com as pecas que ainda estao por
    colocar, pela ordem de colocacao. As pecas nesta lista sao representadas
    pelo simbolo correspondente a letra da peca.
- pecas-colocadas: uma lista com as pecas ja colocadas no tabuleiro.
	 Esta lista deve encontrar-se ordenada da peca mais recente para a mais
	 antiga.
- tabuleiro: o tabuleiro com as posicoes actualmente preenchidas do jogo."
  (pontos 0)
  pecas-por-colocar
  pecas-colocadas
  (tabuleiro (cria-tabuleiro)))

(defun copia-estado (estado)
  "copia-estado: estado -> estado
Este construtor recebe um estado e devolve um novo estado cujo conteudo deve
ser copiado a partir do estado original. O estado devolvido devera garantir
que qualquer alteracao feita ao estado original nao deve ser repercutida no
novo estado e vice-versa."
  (make-estado :pontos (estado-pontos estado)
               :pecas-por-colocar (copy-list (estado-pecas-por-colocar estado))
               :pecas-colocadas   (copy-list (estado-pecas-colocadas   estado))
               :tabuleiro         (copia-tabuleiro (estado-tabuleiro estado))))

(defun estados-iguais-p (estado1 estado2)
  "estados-iguais-p: estado x estado -> logico
Este teste recebe dois estados, e devolve o valor logico verdade se os dois
estados forem iguais (i.e. tiverem o mesmo conteudo) e falso caso contrario."
  (and (equalp (estado-pontos            estado1) (estado-pontos            estado2))
       (equalp (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))
       (equalp (estado-pecas-colocadas   estado1) (estado-pecas-colocadas   estado2))
       (equalp (estado-tabuleiro         estado1) (estado-tabuleiro         estado2))))

(defun estado-final-p (estado)
  "estado-final-p: estado -> logico
Este reconhecedor recebe um estado e devolve o valor logico verdade se
corresponder a um estado final onde o jogador ja nao possa fazer mais jogadas e
falso caso contrario. Um estado e considerado final se o tabuleiro tiver
atingido o topo ou se ja nao existem pecas por colocar."
  (or (tabuleiro-topo-preenchido-p (estado-tabuleiro         estado))
      (null                        (estado-pecas-por-colocar estado))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIPO ACCAO
;;; Este e representado atraves de um par (coluna, peca), que significa que o
;;; objectivo e colocar a peca de forma a sua posicao mais a esquerda fique
;;; nessa coluna.

(defun cria-accao (coluna peca)
  "cria-accao: inteiro x array -> accao
Este construtor recebe um inteiro correspondente a posicao da coluna mais a
esquerda a partir da qual a peca vai ser colocada, e um array com a configuracao
da peca a colocar, e devolve uma nova accao"
  (cons coluna peca))

(defun accao-coluna (accao)
  "accao-coluna: accao -> inteiro
Este selector devolve um inteiro correspondente a coluna mais a esquerda a
partir da qual a peca vai ser colocada."
  (car accao))

(defun accao-peca (accao)
  "accao-peca: accao -> array
Este selector devolve o array com a configuracao geometrica exacta com que vai
ser colocada."
  (cdr accao))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIPO PROBLEMA

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCURA-PP

(defun procura-pp (problema)
  "procura-pp: problema --> lista de accoes
Versao em profundidade primeiro do TREE-SEARCH. Recebe uma estrutura problema e
devolve um lista de accoes que corresponde a accoes a tomar para atingir um
estado objectivo a partir do estado inicial."
  (let ((estado-inicial (problema-estado-inicial problema))
        (solucao        (problema-solucao        problema))
        (accoes         (problema-accoes         problema))
        (resultado      (problema-resultado      problema)))
    (labels
        ((procura-pp-recursivo (estado)
           "procura-pp-recursivo: estado --> lista de accoes"
           (let ((lista-accoes (reverse (funcall accoes estado))))
             ;; Para cada accao possivel, por ordem LIFO, expande o
             ;; estado e verifica se e solucao na geracao. Se for
             ;; solucao devolve uma lista com a accao que a originou; se
             ;; nao, comeca uma procura a partir desse estado.
             (dolist (accao lista-accoes nil)
               (let ((novo-estado (funcall resultado estado accao)))
                 (when (funcall solucao novo-estado)
                   (return-from procura-pp-recursivo
                     (list accao)))
                 (let ((procura (procura-pp-recursivo novo-estado)))
                   ;; Se encontrarmos solucao a partir do estado gerado,
                   ;; juntamos a lista de accoes *devolvida* pela procura a
                   ;; accao que gerou esse estado. Se o resultado da procura
                   ;; for vazio, entao a procura nao encontrou solucao e
                   ;; procede-se a geracao do estado resultante da aplicacao
                   ;; da proxima accao na *lista-accoes*.
                   (when (not (null procura))
                     (return-from procura-pp-recursivo
                       (cons accao procura)))))))))
      (procura-pp-recursivo estado-inicial))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCURA-A*

(defstruct no
  "Esta estrutura representa um estado na arvore de procura. Contem um estado,
o valor da funcao de avaliacao nesse estado (f(n) = g(n) + h(n)) e um caminho,
que e uma lista de accoes que, aplicadas ao estado inicial, culminam no estado
deste no."
  estado avaliacao caminho)

(defun no< (no-1 no-2)
  "no<: no x no --> logico
Comparacao de nos. Um primeiro no e menor que um segundo no se a sua avaliacao
for menor que a do segundo."
  (cond ((null no-1) no-2)
        ((null no-2) no-1)
        (t (< (no-avaliacao no-1)
              (no-avaliacao no-2)))))

(defun procura-A* (problema heuristica)
  "procura-A*: problema x heuristica --> lista de accoes
Recebe uma estrutura do tipo problema e uma funcao heuristica (estado --> inteiro)
e devolve uma lista de accoes que corresponde ao caminho desde o estado inicial
ate a ao estado solucao encontrado. Se nao for encontrada solucao, e devolvido nil."
  (let* ((estado-inicial (problema-estado-inicial problema))
         (solucao        (problema-solucao        problema))
         (accoes         (problema-accoes         problema))
         (resultado      (problema-resultado      problema))
         (g              (problema-custo-caminho  problema))
         (h heuristica)
         (fronteira (list (make-no :estado          estado-inicial
                                   :avaliacao       0
                                   :caminho nil))))
    (loop do
      (let ((no (pop fronteira)))
        (cond ((null no) (return nil))
              ((funcall solucao (no-estado no)) (return (reverse (no-caminho no))))
              (t
               ;; Expande o no e adiciona os nos resultantes a fronteira
               ;; ordenados pela funcao de avaliacao f(n) = g(n) + h(n).
               (let* ((estado (no-estado no))
                      (lista-accoes (funcall accoes estado)))
                 (dolist (accao lista-accoes)
                   (let* ((novo-estado (funcall resultado estado accao))
                          (novo-no (make-no :estado novo-estado
                                            :avaliacao (+ (funcall g novo-estado)
                                                          (funcall h novo-estado))
                                            :caminho (cons accao
                                                           (no-caminho no)))))
                     (setf fronteira
                           (merge 'list
                                  (list novo-no)
                                  fronteira
                                  #'no<)))))))))))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCURA-BEST

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

(load "utils.fas")
