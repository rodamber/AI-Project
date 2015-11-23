(defun heuristica-relevo (estado)
	(let ((pontuacao 0)
		  (tabuleiro (estado-tabuleiro estado)))
		(loop for coluna from 1 upto 9 do
			(incf pontuacao 
				(abs (- (tabuleiro-altura-coluna tabuleiro coluna) 
						(tabuleiro-altura-coluna tabuleiro (1- coluna))))))
		pontuacao))

(defun heuristica-buracos (estado)
	(let ((pontuacao 0)
		  (tabuleiro (estado-tabuleiro estado)))
		(dotimes (coluna 10)
			(loop for linha from (1- (tabuleiro-altura-coluna tabuleiro coluna)) downto 0 do
				(if (not (tabuleiro-preenchido-p tabuleiro linha coluna)) (incf pontuacao))))
		pontuacao))

(defun heuristicas-pontuacao (estado)
	(estado-pontos estado))