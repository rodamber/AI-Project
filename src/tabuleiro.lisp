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
    (dotimes (linha numero-de-linhas 0)
      (when (aref tabuleiro linha coluna)
        (return (- numero-de-linhas linha))))))

(defun tabuleiro-linha-completa-p (tabuleiro linha)
  "tabuleiro-linha-completa-p: tabuleiro x inteiro --> logico
Recebe um tabuleiro e linha, e devolve T se todas as posicoes dessa linha
estiverem preenchidas, e NIL caso contrario."
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
    (when (and (> linha  0)
               (> coluna 0)
               (< linha  (array-dimension tabuleiro 0))
               (< coluna (array-dimension tabuleiro 1))
      (setf (aref tabuleiro linha coluna) t))))

(defun tabuleiro-remove-linha! (tabuleiro linha)
  "tabuleiro-remove-linha!: tabuleiro x inteiro --> {}
Recebe um tabuleiro e linha, e altera o tabuleiro recebido removendo essa linha
do tabuleiro, fazendo com que as linhas por cima da linha removida descam uma
linha. As linhas que estao por baixo da linha removida nao podem ser alteradas.
O valor devolvido por esta funcao nao esta definido."
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
  (dotimes (i (array-dimension tabuleiro 1))
    (when (aref tabuleiro 0 i)
      (return t))))

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

