
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

