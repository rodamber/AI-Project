;; TIPO PROBLEMA

;; Estrutura do problema do jogo
;; - estado-inicial: contem o estado inicial do problema de procura;
;; - solucao: funcao que recebe um estado e devolve T se o estado
;;     for uma solucao para o problema de procura, e nil caso contrario;
;; - accoes: funcao que recebe um estado e devolve uma lista com todas
;;     as accoes que sao possiveis fazer nesse estado;
;; - resultado: funcao que dado um estado e uma accao devolve o estado
;;     sucessor que resulta de executar a accao recebida no estado recebido;
;; - custo-caminho: funcao que dado um estado devolve o custo do caminho
;;     desde o estado inicial ate esse estado.
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

;;	solucao: estado -> logico
;;	Esta funcao recebe um estado, e devolve o valor logico verdade se o estado
;;recebido corresponder a uma solucao, e falso contrario. Um estado do jogo
;;Tetris e considerado solucao se o topo do tabuleiro nao estiver preenchido e
;;se ja nao existem pecas por colocar, ou seja, todas as pecas foram colocadas
;;com sucesso.
(defun solucao (estado)
  (let* ((tabuleiro  (estado-tabuleiro         estado))
         (pecas      (estado-pecas-por-colocar estado))
         (preenchido (tabuleiro-topo-preenchido-p   tabuleiro)))
    (and (not preenchido) (null pecas))))

;;	accoes: estado -> lista de accoes
;;	Esta funcao recebe um estado e devolve uma lista de accoes correspondendo a
;;todas as accoes validas que podem ser feitas com a proxima peca a ser
;;colocada. Uma accao e considerada valida mesmo que faca o jogador perder o
;;jogo.
(defun accoes (estado)
  (let ((lista-accoes nil)
        (n 10))
    (case (first (estado-pecas-por-colocar estado))
      (i (dotimes (i n)
           (setf lista-accoes (cons (cria-accao i peca-i0) lista-accoes)))
         (dotimes (i (- n 3))
           (setf lista-accoes (cons (cria-accao i peca-i1) lista-accoes))))
      (l (dotimes (i (1- n))
           (setf lista-accoes(cons (cria-accao i peca-l0) lista-accoes))
           (setf lista-accoes(cons (cria-accao i peca-l2) lista-accoes)))
         (dotimes (i (- n 2))
           (setf lista-accoes(cons (cria-accao i peca-l1) lista-accoes))
           (setf lista-accoes(cons (cria-accao i peca-l3) lista-accoes))))
      (o (dotimes (i (1- n))
           (setf lista-accoes(cons (cria-accao i peca-o0) lista-accoes))))
      (s (dotimes (i (- n 2))
           (setf lista-accoes(cons (cria-accao i peca-s0) lista-accoes)))
         (dotimes (i (1- n))
           (setf lista-accoes(cons (cria-accao i peca-s1) lista-accoes))))
      (z (dotimes (i (- n 2))
           (setf lista-accoes(cons (cria-accao i peca-z0) lista-accoes)))
         (dotimes (i (1- n))
           (setf lista-accoes(cons (cria-accao i peca-z1) lista-accoes))))
      (t (dotimes (i (- n 2))
           (setf lista-accoes(cons (cria-accao i peca-t0) lista-accoes))
           (setf lista-accoes(cons (cria-accao i peca-t2) lista-accoes)))
         (dotimes (i (1- n))
           (setf lista-accoes(cons (cria-accao i peca-t1) lista-accoes))
           (setf lista-accoes(cons (cria-accao i peca-t3) lista-accoes)))))
    lista-accoes))

;;  resultado: estado x accao -> estado
;;	Esta funcao recebe um estado e uma accao, e devolve um novo estado que
;;  resulta de aplicar a accao recebida no estado original.
(defun resultado (estado accao)
  (setf (estado-pecas-colocadas estado)
        (cons (first (estado-pecas-por-colocar estado))
              (estado-pecas-colocadas estado)))

  (setf (estado-pecas-por-colocar estado)
        (rest (estado-pecas-por-colocar estado)))

  (let* ((tabuleiro        (estado-tabuleiro estado))
         (tabuleiro-altura (array-dimension tabuleiro 0))
         (peca             (accao-peca accao))
         (peca-coluna      (accao-coluna accao))
         (linhas-completas 0))

    (dotimes (linha-tabuleiro tabuleiro-altura)
      (if (verifica-espaco-livre tabuleiro
                                 peca
                                 peca-coluna
                                 linha-tabuleiro)
          (progn (setf (estado-tabuleiro estado)
                       (coloca-peca tabuleiro
                                    peca
                                    peca-coluna
                                    linha-tabuleiro))
                 (setf linha-tabuleiro
                       tabuleiro-altura))))

    (if (tabuleiro-topo-preenchido-p tabuleiro)
        (return-from resultado estado))

    (dotimes (linha-tabuleiro tabuleiro-altura)
      (if (tabuleiro-linha-completa-p tabuleiro
                                      linha-tabuleiro)
          (progn (incf linhas-completas)
                 (tabuleiro-remove-linha! tabuleiro
                                          linha-tabuleiro))))
    (case linhas-completas
      (1 (incf (estado-pontos estado) 100))
      (2 (incf (estado-pontos estado) 300))
      (3 (incf (estado-pontos estado) 500))
      (4 (incf (estado-pontos estado) 800)))

    estado))

(defun verifica-espaco-livre (tabuleiro peca x y)
  (dotimes (linha (array-dimension peca 0) T)
    (dotimes (coluna (array-dimension peca 1))
      (if (and (< (+ linha  y) (array-dimension tabuleiro 0))
               (< (+ coluna x) (array-dimension tabuleiro 1))
               (tabuleiro-preenchido-p tabuleiro (+ linha y) (+ coluna x)))
          (return-from verifica-espaco-livre NIL)))))

(defun coloca-peca (tabuleiro peca x y)
  (dotimes (altura-peca (array-dimension peca 0) tabuleiro)
    (dotimes (largura-peca (array-dimension peca 1))
      (tabuleiro-preenche! tabuleiro (+ y altura-peca) (+ x largura-peca)))))

;;	qualidade: estado -> inteiro
;;	Esta funcao recebe um estado e retorna um valor de quatidade que corresponde ao
;;valor negativo dos pontos ganhos ate ao momento.
(defun qualidade (estado)
  (* -1 (estado-pontos estado)))

;;	custo-oportunidade: estado -> inteiro
;;	Esta funcao, dado um estado, devolve o custo de oportunidade de todas as accoes
;;realizadas ate ao momento, assumindo que e sempre possivel fazer o maximo de pontos
;;por cada peca colocada
(defun custo-oportunidade (estado)
  (- (pontos-maximos(estado-pecas-colocadas estado)) (estado-pontos estado)))

(defun pontos-maximos (lis)
  (cond ((equal (first lis) 'i)
         (+ (pontos-maximos2 (rest lis)) 800))
        ((or (equal (first lis) 'j) (equal(first lis) 'l))
         (+ (pontos-maximos2 (rest lis)) 500))
        ((or (equal (first lis) 's) (equal(first lis) 'z) (equal (first lis) 't)
             (equal(first lis) 'o))
         (+ (pontos-maximos2 (rest lis)) 300))
        (t 0)))
