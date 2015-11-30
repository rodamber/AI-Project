
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIPO ESTADO

(defstruct estado
  "Estrutura do estado do jogo
- pontos: o numero de pontos conseguidos ate ao momento no jogo.
- pecas-por-colocar: uma lista com as pecas que ainda estao por
    colocar, pela ordem de colocacao. As pecas nesta lista sao representadas
    pelo simbolo correspondente a letra da peca.
- pecas-colocadas: uma lista com as pecas ja colocadas no tabuleiro.
	 Esta lista deve encontrar-se ordenada da peca mais recente para a mais
	 antiga.
- tabuleiro: o tabuleiro com as posicoes actualmente preenchidas do jogo."
  (pontos 0)
  pecas-por-colocar
  pecas-colocadas
  (tabuleiro (cria-tabuleiro)))

(defun copia-estado (estado)
  "copia-estado: estado -> estado
Este construtor recebe um estado e devolve um novo estado cujo conteudo deve
ser copiado a partir do estado original. O estado devolvido devera garantir
que qualquer alteracao feita ao estado original nao deve ser repercutida no
novo estado e vice-versa."
  (make-estado :pontos (estado-pontos estado)
               :pecas-por-colocar (copy-list (estado-pecas-por-colocar estado))
               :pecas-colocadas   (copy-list (estado-pecas-colocadas   estado))
               :tabuleiro         (copia-tabuleiro (estado-tabuleiro estado))))

(defun estados-iguais-p (estado1 estado2)
  "estados-iguais-p: estado x estado -> logico
Este teste recebe dois estados, e devolve o valor logico verdade se os dois
estados forem iguais (i.e. tiverem o mesmo conteudo) e falso caso contrario."
  (and (equalp (estado-pontos            estado1) (estado-pontos            estado2))
       (equalp (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))
       (equalp (estado-pecas-colocadas   estado1) (estado-pecas-colocadas   estado2))
       (equalp (estado-tabuleiro         estado1) (estado-tabuleiro         estado2))))

(defun estado-final-p (estado)
  "estado-final-p: estado -> logico
Este reconhecedor recebe um estado e devolve o valor logico verdade se
corresponder a um estado final onde o jogador ja nao possa fazer mais jogadas e
falso caso contrario. Um estado e considerado final se o tabuleiro tiver
atingido o topo ou se ja nao existem pecas por colocar."
  (or (tabuleiro-topo-preenchido-p (estado-tabuleiro         estado))
      (null                        (estado-pecas-por-colocar estado))))
