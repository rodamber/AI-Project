(defun heuristica-relevo (estado)
  "heuristica-relevo: estado --> numero"
  (if (solucao estado)
      0
      (~heuristica-relevo estado)))

(defun heuristica-buracos (estado)
  "heuristica-buracos: estado --> numero"
  (if (solucao estado)
      0
      (~heuristica-buracos estado)))

(defun heuristica-pontuacao (estado)
  "heuristica-pontuacao: estado --> numero"
  (if (solucao estado)
      0
      (~heuristica-pontuacao estado)))

(defun heuristica-pecas-por-colocar (estado)
  "heuristica-pecas-por-colocar: estado --> numero"
  (if (solucao estado)
      0
      (~heuristica-pecas-por-colocar estado)))

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

(defun ~heuristica-buracos (estado)
  (let ((buracos 0)
        (tabuleiro (estado-tabuleiro estado)))
    (dotimes (coluna 10 buracos)
      (incf buracos
            (tabuleiro-coluna-buracos tabuleiro
                                      coluna)))))

(defun ~heuristica-pontuacao (estado)
  (estado-pontos estado))

(defun ~heuristica-pecas-por-colocar (estado)
  (estado-pecas-por-colocar estado))

