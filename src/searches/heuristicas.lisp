(defun custo-altura-agregada (estado)
  "custo-altura-agregada --> numero"
  (let ((tabuleiro (estado-tabuleiro estado))
        (altura-agregada 0))
    (dotimes (coluna 10 altura-agregada)
      (incf altura-agregada
            (tabuleiro-altura-coluna tabuleiro
                                     coluna)))))

(defun custo-buracos (estado)
  "custo-buracos: estado --> numero"
  (let ((buracos 0)
        (tabuleiro (estado-tabuleiro estado)))
    (dotimes (coluna 10 buracos)
      (incf buracos
            (tabuleiro-coluna-buracos tabuleiro
                                      coluna)))))

(defun custo-pecas-colocadas (estado)
  "custo-pecas-por-colocar: estado --> numero"
  (list-length (estado-pecas-colocadas estado)))

(defun custo-relevo (estado)
  "custo-relevo: estado --> numero"
  (let ((relevo 0)
        (tabuleiro (estado-tabuleiro estado)))
    (loop for coluna from 1 upto 9 do
      (let ((a1 (tabuleiro-altura-coluna tabuleiro coluna))
            (a2 (tabuleiro-altura-coluna tabuleiro (1- coluna))))
        (incf relevo (abs (- a1 a2)))))
    relevo))

(defun heuristica-pontuacao-1 (estado)
  "heuristica-pontuacao: estado --> numero"
  (let* ((numero-pecas (+ (list-length (estado-pecas-colocadas estado))
                         (list-length (estado-pecas-por-colocar estado))))
         (pontuacao-maxima (* 80 numero-pecas)))
    (- pontuacao-maxima (estado-pontos estado))))

(defun heuristica-pontuacao-2 (estado)
  "heuristica-pontuacao: estado --> numero"
  (- (+ (pontos-maximos (estado-pecas-por-colocar estado))
        (pontos-maximos (estado-pecas-colocadas   estado)))
     (estado-pontos estado)))

(defun heuristica-altura (estado)
  (let* ((tabuleiro (estado-tabuleiro estado))
         (alturas-colunas (loop for c from 0 to 9 collect (tabuleiro-altura-coluna tabuleiro c))))
    (apply #'max alturas-colunas)))

