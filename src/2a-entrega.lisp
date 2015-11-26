;;; Implementacao do tipo tabuleiro
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
  (let ((numero-de-linhas (array-dimension tabuleiro 0)))
    (loop for linha from (1- numero-de-linhas) downto 0
          do (when (tabuleiro-preenchido-p tabuleiro linha coluna)
               (return-from tabuleiro-altura-coluna (1+ linha))))
    0))

(defun tabuleiro-linha-completa-p (tabuleiro linha)
  "tabuleiro-linha-completa-p: tabuleiro x inteiro --> logico
Recebe um tabuleiro e linha, e devolve T se todas as posicoes dessa linha
estiverem preenchidas, e NIL caso contrario."
  (let ((numero-de-colunas (array-dimension tabuleiro 1)))
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
             (< linha  (array-dimension tabuleiro 0))
             (< coluna (array-dimension tabuleiro 1))
             (setf (aref tabuleiro linha coluna) t))))

(defun tabuleiro-remove-linha! (tabuleiro linha)
  "tabuleiro-remove-linha!: tabuleiro x inteiro --> {}
Recebe um tabuleiro e linha, e altera o tabuleiro recebido removendo essa linha
do tabuleiro, fazendo com que as linhas por cima da linha removida descam uma
linha. As linhas que estao por baixo da linha removida nao podem ser alteradas.
O valor devolvido por esta funcao nao esta definido."
  (let ((numero-de-linhas (array-dimension tabuleiro 0)))
    (loop for i from linha to (- numero-de-linhas 2) do
      ;; Trocam-se as linhas de modo a que a linha a ser removida se encontre no
      ;; topo do tabuleiro no final do ciclo e que as suas superiores descam uma
      ;; linha. Se a linha for a do topo, nao ha necessidade de fazer trocas.
      (tabuleiro-troca-linhas! tabuleiro i (1+ i)))
    (dotimes (i (array-dimension tabuleiro 1))
      ;; No final, "remove-se" a linha do topo.
      (setf (aref tabuleiro (1- numero-de-linhas) i) nil))))

(defun tabuleiro-troca-linhas! (tabuleiro linha-1 linha-2)
  "tabuleiro-troca-linhas!: tabuleiro x inteiro x inteiro --> tabuleiro
Recebe um tabuleiro e duas linhas e troca os conteudos dessas linhas."
  (dotimes (i (array-dimension tabuleiro 1))
    (let ((x (aref tabuleiro linha-1 i))
          (y (aref tabuleiro linha-2 i)))
      (setf (aref tabuleiro linha-1 i) y)
      (setf (aref tabuleiro linha-2 i) x))))

(defun tabuleiro-remove-linhas-completas! (tabuleiro)
  "tabuleiro-remove-linhas-completas!: tabuleiro --> inteiro 
Remove todas as linhas do tabuleiro que estejam completas. Devolve o numero de
linhas removidas."
  (let ((numero-de-linhas (array-dimension tabuleiro 0))
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
  (let ((numero-de-linhas  (array-dimension tabuleiro 0))
        (numero-de-colunas (array-dimension tabuleiro 1)))
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
                                     (list 
    algoritmo
    algoritmo(cria-accao i
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
           (setf (estado-pecas-colocadas no
    algoritmo
    algoritmovo-estado)
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

(defun peca->letra (peca)
  "peca->letra: peca --> simbolo
Dada uma peca, devolve a letra que representa a peca."
  (cond ((member peca (list peca-i0 peca-i1)                 :test #'equalp) 'i)
        ((member peca (list peca-l0 peca-l1 peca-l2 peca-l3) :test #'equalp) 'l)
        ((member peca (list peca-j0 peca-j1 peca-j2 peca-j3) :test #'equalp) 'j)
        ((member peca (list peca-o0)                         :test #'equalp) 'o)
        ((member peca (list peca-s0 peca-s1)                 :test #'equalp) 's)
        ((member peca (list peca-z0 peca-z1)                 :test #'equalp) 'z)
        ((member peca (list peca-t0 peca-t1 peca-t2 peca-t3) :test #'equalp) 't)))

(defun qualidade (estado)
  "qualidade: estado -> inteiro
Esta funcao recebe um estado e retorna um valor de quatidade que corresponde ao
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
(defun heuristica-relevo (estado)
  "heuristica-relevo: estado --> numero"
  (heuristica-auxiliar #'~heuristica-relevo estado))

(defun heuristica-buracos (estado)
  "heuristica-buracos: estado --> numero"
  (heuristica-auxiliar #'~heuristica-buracos estado))

(defun heuristica-pontuacao (estado)
  "heuristica-pontuacao: estado --> numero"
  (heuristica-auxiliar #'~heuristica-pontuacao estado))

(defun heuristica-pecas-por-colocar (estado)
  "heuristica-pecas-por-colocar: estado --> numero"
  (heuristica-auxiliar #'~heuristica-pecas-por-colocar estado))

(defun heuristica-altura-agregada (estado)
  "heuristica-altura-agregada: estado --> numero"
  (heuristica-auxiliar #'~heuristica-altura-agregada estado))

(defun heuristica-altura-4 (estado)
  "heuristica-altura-4: estado --> numero"
  (heuristica-auxiliar #'~heuristica-altura-4 estado))

(defun heuristica-altura-modulo-4 (estado)
  "heuristica-altura-4: estado --> numero"
  (heuristica-auxiliar #'~heuristica-altura-modulo-4 estado))

(defun heuristica-auxiliar (~heuristica estado)
  "heuristica-auxiliar: heuristica x estado --> numero
Recebe uma funcao quase heuristica (que nao devolve 0 para um estado solucao) e
um estado. Devolve 0 caso o estado seja um estado solucao. Caso contrario,
devolve o resultado de aplicar a funcao quase heuristica ao estado recebido."
  (if (solucao estado)
      0
      (funcall ~heuristica estado)))

;-------------------------------------------------------------------------------

(defun ~heuristica-relevo (estado)
  "~heuristica-relevo: estado --> numero"
  (let ((relevo 0)
        (tabuleiro (estado-tabuleiro estado)))
    (loop for coluna from 1 upto 9 do
      (let ((a1 (tabuleiro-altura-coluna tabuleiro coluna))
            (a2 (tabuleiro-altura-coluna tabuleiro (1- coluna))))
        (incf relevo (abs (- a1 a2)))))
    relevo))

(defun ~heuristica-altura-4 (estado)
  "~heuristica-altura-4: estado --> numero
  recompensa pela altura ser um multiplo de 4"
  (let* ((tabuleiro (estado-tabuleiro estado))
        (altura-max (apply #'max (loop for c from 0 to 9 collect (tabuleiro-altura-coluna tabuleiro c)))))
    (ceiling (/ altura-max 4))))

(defun ~heuristica-altura-modulo-4 (estado)
  "~heuristica-altura-4: estado --> numero
  recompensa pela altura ser um multiplo de 4"
  (let* ((tabuleiro (estado-tabuleiro estado))
        (altura-max (apply #'max (loop for c from 0 to 9 collect (tabuleiro-altura-coluna tabuleiro c)))))
    (mod altura-max 4))))

(defun ~heuristica-buracos (estado)
  "~heuristica-buracos: estado --> numero"
  (let ((buracos 0)
        (tabuleiro (estado-tabuleiro estado)))
    (dotimes (coluna 10 buracos)
      (incf buracos
            (tabuleiro-coluna-buracos tabuleiro
                                      coluna)))))

(defun ~heuristica-pontuacao (estado)
  "~heuristica-pontuacao: estado --> numero"
  (- (+ (pontos-maximos (estado-pecas-colocadas    estado))
        (pontos-maximos (estado-pecas-por-colocar estado)))
     (estado-pontos estado)))

(defun ~heuristica-pecas-por-colocar (estado)
  "~heuristica-pecas-por-colocar: estado --> numero"
  (list-length (estado-pecas-por-colocar estado)))

(defun ~heuristica-altura-agregada (estado)
  "~heuristica-altura-agregada --> numero"
  (let ((tabuleiro (estado-tabuleiro estado))
        (altura-agregada 0))
    (dotimes (coluna 10 altura-agregada)
      (incf altura-agregada
            (tabuleiro-altura-coluna tabuleiro
                                     coluna)))))

(defun h-rbppa (estado)
  (labels
      ((~h (estado)
         (+ (funcall #'~heuristica-altura-agregada   estado)
            (funcall #'~heuristica-pecas-por-colocar estado)
            (funcall #'~heuristica-pontuacao         estado)
            (funcall #'~heuristica-buracos           estado))))
    (funcall #'heuristica-auxiliar #'~h estado)))
(defun procura-pp (problema)
  "procura-pp: problema --> lista de accoes"
  (let ((estado-inicial (problema-estado-inicial problema))
        (solucao        (problema-solucao        problema))
        (accoes         (problema-accoes         problema))
        (resultado      (problema-resultado      problema)))
    (labels
        ((procura-pp-recursivo (estado)
           "procura-pp-recursivo: estado --> lista de accoes"
           (let ((lista-accoes (reverse (funcall accoes estado))))
             (if (null lista-accoes)
                 (return-from procura-pp-recursivo
                   nil)
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
                           (cons accao procura))))))))))
      (procura-pp-recursivo estado-inicial))))

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
  "procura-A*: problema x heuristica --> lista de accoes"
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
      (let* ((no     (pop fronteira))
             (estado (no-estado no)))
        (cond ((null no) (return nil))
              ((funcall solucao (no-estado no)) (return (reverse (no-caminho no))))
              (t
               ;; Expande o no e adiciona os nos resultantes a fronteira
               ;; ordenados pela funcao de avaliacao f(n) = g(n) + h(n).
               (let ((lista-accoes (funcall accoes estado)))
                 (dolist (accao lista-accoes)
                   (let* ((novo-estado (funcall resultado estado accao))
                          (novo-no (make-no :estado novo-estado
                                            :avaliacao (+ (funcall g novo-estado)
                                                          (funcall h novo-estado))
                                            :caminho (cons accao
                                                           (no-caminho no)))))
                     ;; Insere o no na fronteira.
                     (setf fronteira
                           (merge 'list (list novo-no) fronteira #'no<)))))))))))

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

(defparameter lista-accoes-1 '((0 . #2A((T T T) (NIL T NIL)))
                               (1 . #2A((T) (T) (T) (T)))
                               (1 . #2A((NIL NIL T) (T T T)))
                               (3 . #2A((NIL T NIL) (T T T)))
                               (6 . #2A((NIL T T) (T T NIL)))))

(load "utils.fas")

(setq et1 (make-estado :tabuleiro (cria-tabuleiro) :pecas-por-colocar (random-pecas 25)))

(setq pt1
  (make-problema :estado-inicial et1
                 :solucao #'solucao
                 :accoes #'accoes
                 :resultado #'resultado
                 :custo-caminho #'custo-oportunidade))

(defun teste-com-heuristica (funcao-procura heuristica numero-de-pecas)
  (labels ((procura (problema)
             (funcall funcao-procura problema heuristica)))
    (teste #'procura numero-de-pecas)))

(defun teste (funcao-procura heuristica numero-de-pecas &key (solucao #'solucao) (accoes #'accoes) (resultado #'resultado) (custo-caminho #'custo-oportunidade))
  (let* ((estado (make-estado :tabuleiro (cria-tabuleiro)
                              :pecas-por-colocar (random-pecas numero-de-pecas)))
         (problema (make-problema :estado-inicial estado
                                  :solucao solucao
                                  :accoes accoes
                                  :resultado resultado
                                  :custo-caminho custo-caminho)))
    (executa-jogadas estado
                     (time (funcall funcao-procura
                                    problema heuristica)))))

(defun teste-pp (numero-de-pecas)
  (funcall #'teste #'procura-pp numero-de-pecas))

(defun teste-A* (heuristica numero-de-pecas)
  (funcall #'teste-com-heuristica
           #'procura-A*
           heuristica
           numero-de-pecas))

(defun get-time (x)
  (let ((t0 (get-internal-run-time)))
    (progn
      x
      (- (get-internal-run-time)
         t0))))

(defun time-mean (form times)
  (float (apply #'mean
                (loop for i from 1 upto times
                      collect (get-time form)))))

(defun mean (&rest sequence)
  (if (null sequence)
      nil
      (/ (reduce #'+ sequence) (length sequence))))