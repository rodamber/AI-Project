(defun cria-accao (coluna peca) (cons coluna peca))
(defun accao-coluna (accao) (car accao))
(defun accao-peca (accao) (cdr accao))
