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
