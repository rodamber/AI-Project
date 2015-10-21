;; TIPO ESTADO

;; Estrutura do estado do jogo
;; - pontos: o número de pontos conseguidos até ao momento no jogo.
;; - pecas-por-colocar: uma  lista  com  as  peças  que  ainda  estão por
;;     colocar,  pela  ordem  de colocação. As peças nesta lista são
;;     representadas pelo símbolo correspondente à letra da peça.
;; - pecas-colocadas: uma  lista  com  as  peças  já  colocadas  no  tabuleiro.
;;		 Esta lista deve encontrar-se ordenada da peça mais recente para a mais antiga.
;; - tabuleiro: o tabuleiro com as posições actualmente preenchidas do jogo.

(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

;; copia-estado: estado -> estado
;; Este construtor recebe um estado e devolve um novo estado cujo conteúdo deve 
;; ser copiado a partir  do  estado  original.  O  estado devolvido deverá
;; garantir  que  qualquer  alteração  feita  ao estado original não deve ser 
;; repercutida no novo estado e vice-versa.

(defun copia-estado (estado)
  (copy-estado estado)
)

;; estados-iguais-p: estado x estado -> logico
;; Este teste recebe dois estados, e devolve o valor lógico verdade se os dois
;; estados forem iguais (i.e. tiverem o mesmo conteúdo) e falso caso contrário.

(defun estados-iguais-p (estado1 estado2)
  (and
    (equal (estado-pontos estado1) (estado-pontos estado2))
    (equal (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))
    (equal (estado-pecas-colocadas estado1) (estado-pecas-colocadas estado2))
    (equal (estado-tabuleiro estado1) (estado-tabuleiro estado2))
  )
)

;; estado-final-p: estado -> logico
;; Este  reconhecedor  recebe  um  estado  e devolve o  valor  lógico  verdade
;; se  corresponder  a  um estado final onde o jogador já não possa fazer mais
;; jogadas e falso caso contrário. Um estado é considerado final se o tabuleiro
;; tiver atingido o topo ou se já não existem peças por colocar.

(defun estado-final-p (estado)
  (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
)
