;; TIPO ACCAO

;; cria-accao: inteiro x array -> accao
;; Este construtor recebe um inteiro correspondente à posição da coluna mais à 
;; esquerda a partir da qual a peça vai ser colocada, e um array com a 
;; configuração da peça a colocar, e devolve uma nova acção

(defun cria-accao (coluna peca)
	(cons coluna peca)
)

;; accao-coluna: accao -> inteiro
;; Este selector devolve um  inteiro  correspondente  à coluna mais  à  esquerda 
;; a  partir  da  qual  a peça vai ser colocada.
 
(defun accao-coluna (accao)
	(car accao)
)

;; accao-peca: accao -> array
;; Este selector devolve o array com a configuração geométrica exacta com que 
;; vai ser colocada.

(defun accao-peca (accao)
	(cdr accao)
)
