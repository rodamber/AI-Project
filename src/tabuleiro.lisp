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

         (altura-peca  (array-dimension peca 0))
         (largura-peca (array-dimension peca 1))

         ;; Lista com as alturas das colunas abrangidas pela peca. Por exemplo,
         ;; uma peca com largura 3 a ser colocada na coluna 2 abrange as colunas
         ;; 2, 3 e 4 do tabuleiro.
         (alturas-colunas (loop for c from coluna to (1- (+ coluna largura-peca))
                                collect (tabuleiro-altura-coluna tabuleiro c)))
         ;; Sabendo qual a coluna mais alta das colunas abrangidas pela peca, a
         ;; linha onde a porcao inferior da peca sera colocada sera entao
         ;; determinada por essa altura e pela posicao da parte mais baixa da
         ;; peca correspondente a essa coluna.
         (indice-coluna-mais-alta (max-indice  alturas-colunas))
         (altura-coluna-mais-alta (apply #'max alturas-colunas))
         (deslocamento            (dotimes (i altura-peca)
                                    (when (aref peca i indice-coluna-mais-alta)
                                      (return i))))
         (linha                   (- altura-coluna-mais-alta deslocamento)))
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

(defun max-indice (lista)
  "max-indice: lista --> inteiro
Recebe uma lista e devolve o indice do elemento maximo dessa lista."
  (let ((max    (apply #'max lista))
        (indice 0))
    (dolist (x lista)
      (if (= x max)
          (return indice)
        (incf indice)))))
