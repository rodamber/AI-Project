(defun heuristica-relevo (estado)
  "heuristica-relevo: estado --> numero"
  (heuristica-auxiliar ~heuristica-relevo estado))

(defun heuristica-buracos (estado)
  "heuristica-buracos: estado --> numero"
  (heuristica-auxiliar ~heuristica-buracos estado))

(defun heuristica-pontuacao (estado)
  "heuristica-pontuacao: estado --> numero"
  (heuristica-auxiliar ~heuristica-pontuacao estado))

(defun heuristica-pecas-por-colocar (estado)
  "heuristica-pecas-por-colocar: estado --> numero"
  (heuristica-auxiliar ~heuristica-pecas-por-colocar estado))

(defun heuristica-altura-agregada (estado)
  "heuristica-altura-agregada: estado --> numero"
  (heuristica-auxiliar ~heuristica-altura-agregada estado))

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
  (estado-pontos estado))

(defun ~heuristica-pecas-por-colocar (estado)
  "~heuristica-pecas-por-colocar: estado --> numero"
  (estado-pecas-por-colocar estado))

(defun ~heuristica-altura-agregada (estado)
  "~heuristica-altura-agregada --> numero"
  (let ((tabuleiro (estado-tabuleiro estado))
        (altura-agregada 0))
    (dotimes (coluna 10 altura-agregada)
      (incf altura-agregada
            (tabuleiro-altura-coluna tabuleiro
                                     coluna)))))

