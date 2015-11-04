;; Grupo 48 - 78941 Micael Batista, 78960 Francisco Besteiro, 78942 Rodrigo Bernardo


;; TIPO ACCAO

;; cria-accao: inteiro x array -> accao
;; Este construtor recebe um inteiro correspondente a posicao da coluna mais a
;; esquerda a partir da qual a peca vai ser colocada, e um array com a
;; configuracao da peca a colocar, e devolve uma nova accao

(defun cria-accao (coluna peca)
  (cons coluna peca))

;; accao-coluna: accao -> inteiro
;; Este selector devolve um inteiro correspondente a coluna mais a esquerda a
;; partir da qual a peca vai ser colocada.

(defun accao-coluna (accao)
  (car accao))

;; accao-peca: accao -> array
;; Este selector devolve o array com a configuracao geometrica exacta com que
;; vai ser colocada.

(defun accao-peca (accao)
  (cdr accao))
;; TIPO ESTADO

;; Estrutura do estado do jogo
;; - pontos: o numero de pontos conseguidos ate ao momento no jogo.
;; - pecas-por-colocar: uma  lista  com  as  pecas  que  ainda  estao por
;;     colocar,  pela  ordem  de colocacao. As pecas nesta lista sao
;;     representadas pelo simbolo correspondente a letra da peca.
;; - pecas-colocadas: uma  lista  com  as  pecas  ja  colocadas  no  tabuleiro.
;;		 Esta lista deve encontrar-se ordenada da peca mais recente para a mais antiga.
;; - tabuleiro: o tabuleiro com as posicoes actualmente preenchidas do jogo.
(defstruct estado
  (pontos 0)
  pecas-por-colocar
  pecas-colocadas
  (tabuleiro (cria-tabuleiro)))

;; copia-estado: estado -> estado
;; Este construtor recebe um estado e devolve um novo estado cujo conteudo deve
;; ser copiado a partir do estado original. O estado devolvido devera garantir
;; que qualquer alteracao feita ao estado original nao deve ser repercutida no
;; novo estado e vice-versa.
(defun copia-estado (estado)
  (copy-estado estado))

;; estados-iguais-p: estado x estado -> logico
;; Este teste recebe dois estados, e devolve o valor logico verdade se os dois
;; estados forem iguais (i.e. tiverem o mesmo conteudo) e falso caso contrario.
(defun estados-iguais-p (estado1 estado2)
  (and (equalp (estado-pontos            estado1) (estado-pontos            estado2))
       (equalp (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))
       (equalp (estado-pecas-colocadas   estado1) (estado-pecas-colocadas   estado2))
       (equalp (estado-tabuleiro         estado1) (estado-tabuleiro         estado2))))

;; estado-final-p: estado -> logico
;; Este reconhecedor recebe um estado e devolve o valor logico verdade se
;; corresponder a um estado final onde o jogador ja nao possa fazer mais jogadas
;; e falso caso contrario. Um estado e considerado final se o tabuleiro tiver
;; atingido o topo ou se ja nao existem pecas por colocar.
(defun estado-final-p (estado)
  (or (tabuleiro-topo-preenchido-p (estado-tabuleiro         estado))
      (null                        (estado-pecas-por-colocar estado))))
      
;; TIPO PROBLEMA

;; Estrutura do problema do jogo
;; - estado-inicial: contem o estado inicial do problema de procura;
;; - solucao: funcao que recebe um estado e devolve T se o estado
;;     for uma solucao para o problema de procura, e nil caso contrario;
;; - accoes: funcao que recebe um estado e devolve uma lista com todas
;;	   as accoes que são possiveis fazer nesse estado;
;; - resultado: funcao que dado um estado e uma accao devolve o estado 
;;     sucessor que resulta de executar a accao recebida no estado recebido;
;; - custo-caminho: funcao que dado um estado devolve o custo do caminho
;;     desde o estado inicial ate esse estado.

(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

;;	solucao: estado -> logico
;;	Esta funcao recebe um estado, e devolve o valor logico verdade se o estado
;;recebido corresponder a uma solucao, e falso contrario. Um estado do jogo
;;Tetris e considerado solucao se o topo do tabuleiro nao estiver preenchido e
;;se ja nao existem pecas por colocar, ou seja, todas as pecas foram colocadas
;;com sucesso.

(defun solucao (estado)
  (let* ((tabuleiro  (estado-tabuleiro         estado))
         (pecas      (estado-pecas-por-colocar estado))
         (preenchido (tabuleiro-topo-preenchido-p   tabuleiro)))
    (and (not preenchido) (null pecas))))

;;	accoes: estado -> lista de accoes
;;	Esta funcao recebe um estado e devolve uma lista de accoes correspondendo a
;;todas as accoes validas que podem ser feitas com a proxima peca a ser colocada.
;;Uma accao e considerada valida mesmo que faca o jogador perder o jogo.

(defun accoes (estado)
  (let ((lista-accoes nil) (n 10))
  (case  (first (estado-pecas-por-colocar estado))
  	( 'i
      (dotimes (i n)
        (setf lista-accoes (cons (cria-accao i peca-i0) lista-accoes)))
      (dotimes (i (- n 3))
        (setf lista-accoes (cons (cria-accao i peca-i1) lista-accoes))))
        
  	( 'l
      (dotimes (i (1- n))
        (setf lista-accoes(cons (cria-accao i peca-l0) lista-accoes))
        (setf lista-accoes(cons (cria-accao i peca-l2) lista-accoes)))
      (dotimes (i (- n 2))
        (setf lista-accoes(cons (cria-accao i peca-l1) lista-accoes))
        (setf lista-accoes(cons (cria-accao i peca-l3) lista-accoes))))
        
    ( 'o
      (dotimes (i (1- n))
        (setf lista-accoes(cons (cria-accao i peca-o0) lista-accoes))))
        
    ( 's
      (dotimes (i (- n 2))
        (setf lista-accoes(cons (cria-accao i peca-s0) lista-accoes)))
      (dotimes (i (1- n))
        (setf lista-accoes(cons (cria-accao i peca-s1) lista-accoes))))
     
		( 'z
      (dotimes (i (- n 2))
        (setf lista-accoes(cons (cria-accao i peca-z0) lista-accoes)))
      (dotimes (i (1- n))
      	(setf lista-accoes(cons (cria-accao i peca-z1) lista-accoes))))
        
    ( 't
      (dotimes (i (- n 2))
        (setf lista-accoes(cons (cria-accao i peca-t0) lista-accoes))
        (setf lista-accoes(cons (cria-accao i peca-t2) lista-accoes)))
      (dotimes (i (1- n))
        (setf lista-accoes(cons (cria-accao i peca-t1) lista-accoes))
        (setf lista-accoes(cons (cria-accao i peca-t3) lista-accoes)))))
    lista-accoes))
    
    

;; 	resultado: estado x accao -> estado
;;	Esta funcao recebe um estado e uma accao, e devolve um novo estado que resulta
;;  de aplicar a accao recebida no estado original.

(defun resultado (estado accao)
  (setf (estado-pecas-colocadas estado) (cons (first (estado-pecas-por-colocar estado)) (estado-pecas-colocadas estado)))
  (setf (estado-pecas-por-colocar estado) (rest (estado-pecas-por-colocar estado)))
  
  (let* ((tabuleiro (estado-tabuleiro estado))
        (tabuleiro-altura (array-dimension tabuleiro 0))
        (peca (accao-peca accao))
        (peca-coluna (accao-coluna accao))
        (linhas-completas 0))
        
    (dotimes (linha-tabuleiro tabuleiro-altura)
      (if (verifica-espaco-livre tabuleiro peca peca-coluna linha-tabuleiro)
        (progn (setf (estado-tabuleiro estado) (coloca-peca tabuleiro peca peca-coluna linha-tabuleiro))
               (setf linha-tabuleiro tabuleiro-altura))))
               
    (if (tabuleiro-topo-preenchido-p tabuleiro)
      (return-from resultado estado))  
      
    (dotimes (linha-tabuleiro tabuleiro-altura)
      (if (tabuleiro-linha-completa-p tabuleiro linha-tabuleiro)
        (progn (incf linhas-completas)
          (tabuleiro-remove-linha! tabuleiro linha-tabuleiro)))) 
    (case linhas-completas
      (1 (incf (estado-pontos estado) 100))
      (2 (incf (estado-pontos estado) 300))
      (3 (incf (estado-pontos estado) 500))
      (4 (incf (estado-pontos estado) 800)))
      
    estado))
                                     
(defun verifica-espaco-livre (tabuleiro peca x y)
  (dotimes (linha (array-dimension peca 0) T)
    (dotimes (coluna (array-dimension peca 1))
      (if (and (< (+ linha y) (array-dimension tabuleiro 0))
               (< (+ coluna x) (array-dimension tabuleiro 1))
               (tabuleiro-preenchido-p tabuleiro (+ linha y) (+ coluna x))) 
        (return-from verifica-espaco-livre NIL)))))

(defun coloca-peca (tabuleiro peca x y)
  (dotimes (altura-peca (array-dimension peca 0) tabuleiro)
    (dotimes (largura-peca (array-dimension peca 1))
      (tabuleiro-preenche! tabuleiro (+ y altura-peca) (+ x largura-peca)))))
    

;;	qualidade: estado -> inteiro
;;	Esta funcao recebe um estado e retorna um valor de quatidade que corresponde ao
;;valor negativo dos pontos ganhos ate ao momento.

(defun qualidade (estado)
  (* -1 (estado-pontos estado)))

;;	custo-oportunidade: estado -> inteiro
;;	Esta funcao, dado um estado, devolve o custo de oportunidade de todas as accoes
;;realizadas ate ao momento, assumindo que e sempre possivel fazer o maximo de pontos
;;por cada peca colocada

(defun custo-oportunidade (estado)
  (- (pontos-maximos(estado-pecas-colocadas estado)) (estado-pontos estado)))

  
  (defun pontos-maximos (lis)
  (cond ((equal (first lis) 'i)
            (+ (pontos-maximos2 (rest lis)) 800))
        ((or (equal (first lis) 'j) (equal(first lis) 'l))
            (+ (pontos-maximos2 (rest lis)) 500))
        ((or (equal (first lis) 's) (equal(first lis) 'z) (equal (first lis) 't) 
             (equal(first lis) 'o))
            (+ (pontos-maximos2 (rest lis)) 300))
        (t 0)))

  
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
  (aref tabuleiro (- (array-dimension tabuleiro 0) linha 1) coluna))

(defun tabuleiro-altura-coluna (tabuleiro coluna)
  "tabuleiro-altura-coluna: tabuleiro x inteiro --> inteiro
Recebe um tabuleiro e coluna, e devolve a posicao mais alta dessa coluna que
esteja preenchida."
  (let ((numero-de-linhas (array-dimension tabuleiro 0)))
    (dotimes (linha numero-de-linhas 0)
      (when (tabuleiro-preenchido-p tabuleiro linha coluna)
        (return (- numero-de-linhas linha))))))

(defun tabuleiro-linha-completa-p (tabuleiro linha)
  "tabuleiro-linha-completa-p: tabuleiro x inteiro --> logico
Recebe um tabuleiro e linha, e devolve T se todas as posicoes dessa linha
estiverem preenchidas, e NIL caso contrario."
  (setf linha (- (array-dimension tabuleiro 0) linha 1))
  (let ((numero-de-colunas (array-dimension tabuleiro 1)))
    (dotimes (coluna numero-de-colunas t)
      (when (not (aref tabuleiro linha coluna))
        (return nil)))))

(defun tabuleiro-preenche! (tabuleiro linha coluna)
  "tabuleiro-preenche!: tabuleiro x inteiro x inteiro --> {}
Recebe um tabuleiro, linha e coluna, e altera o tabuleiro para na posicao
correspondente a linha e coluna passar a estar preenchido. Se o numero de linha
e de coluna recebidos nao forem validos (i.e. nao estiverem entre 0 e 17, e 0 e
9), nada e feito. O valor devolvido por esta funcao nao esta definido"
    (setf linha (- (array-dimension tabuleiro 0) linha 1))
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
  (setf linha (- (array-dimension tabuleiro 0) linha 1))
  (dotimes (i linha)
    ;; Trocam-se as linhas de modo a que a linha a ser removida se encontre no
    ;; topo do tabuleiro no final do ciclo e que as suas superiores descam uma
    ;; linha.
    (tabuleiro-troca-linhas! tabuleiro (- linha i) (- linha i 1)))
  (dotimes (i (array-dimension tabuleiro 1))
    ;; No final, "remove-se" a linha do topo.
    (setf (aref tabuleiro 0 i) nil)))

(defun tabuleiro-troca-linhas! (tabuleiro linha-1 linha-2)
  "tabuleiro-troca-linhas!: tabuleiro x inteiro x inteiro --> tabuleiro
Recebe um tabuleiro e duas linhas e troca os conteudos dessas linhas."
    (dotimes (i (array-dimension tabuleiro 1))
      (let ((x (aref tabuleiro linha-1 i))
            (y (aref tabuleiro linha-2 i)))
        (setf (aref tabuleiro linha-1 i) y)
        (setf (aref tabuleiro linha-2 i) x))))

(defun tabuleiro-topo-preenchido-p (tabuleiro)
  "tabuleiro-topo-preenchido-p: tabuleiro --> logico
Recebe um tabuleiro e devolve T se existir alguma posicao na linha do topo do
tabuleiro (linha 17) que esteja preenchida, e NIL caso contrario."
  (dotimes (i (array-dimension tabuleiro 1) nil)
    (when (tabuleiro-preenchido-p tabuleiro (1- (array-dimension tabuleiro 0)) i)
      (return t))))

(defun tabuleiros-iguais-p (tab1 tab2)
  "tabuleiros-iguais-p: tabuleiro x tabuleiro --> logico
Devolve T se os dois tabuleiros forem iguais (i.e. tiverem o mesmo conteudo), e
NIL caso contrario"
  (equalp tab1 tab2))

(defun tabuleiro->array (tabuleiro) ;; TALK A BOOT IT
  "tabuleiro->array: tabuleiro --> array
Recebe um tabuleiro e devolve um novo array com 18 linhas e 10 colunas, que para
cada linha e coluna devera conter o valor logico correspondente a cada posicao
do tabuleiro. Nenhuma alteracao no tabuleiro tem repercussao no array devolvido
e vice-versa."
  (copia-tabuleiro tabuleiro))

(defun array->tabuleiro (array) ;; TALK A BOOT IT
  "array->tabuleiro: array --> tabuleiro
Recebe um array com 18 linhas e 10 colunas cujas posicoes tem o valor logico T
ou NIL, e constroi um novo tabuleiro com o conteudo do array recebido. Nenhuma
alteracao no array original tem repercussao no tabuleiro devolvido e
vice-versa."
  (copia-array2D array))