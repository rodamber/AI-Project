
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCURA-A*

(defstruct no
  "Esta estrutura representa um estado na arvore de procura. Contem um estado,
o valor da funcao de avaliacao nesse estado (f(n) = g(n) + h(n)) e um caminho,
que e uma lista de accoes que, aplicadas ao estado inicial, culminam no estado
deste no."
  estado avaliacao caminho)

(defun no< (no-1 no-2)
  "no<: no x no --> logico
Comparacao de nos. Um primeiro no e menor que um segundo no se a sua avaliacao
for menor que a do segundo."
  (cond ((null no-1) no-2)
        ((null no-2) no-1)
        (t (< (no-avaliacao no-1)
              (no-avaliacao no-2)))))

(defun procura-A* (problema heuristica)
  "procura-A*: problema x heuristica --> lista de accoes
Recebe uma estrutura do tipo problema e uma funcao heuristica (estado --> inteiro)
e devolve uma lista de accoes que corresponde ao caminho desde o estado inicial
ate a ao estado solucao encontrado. Se nao for encontrada solucao, e devolvido nil."
  (let* ((estado-inicial (problema-estado-inicial problema))
         (solucao        (problema-solucao        problema))
         (accoes         (problema-accoes         problema))
         (resultado      (problema-resultado      problema))
         (g              (problema-custo-caminho  problema))
         (h heuristica)
         (fronteira (list (make-no :estado          estado-inicial
                                   :avaliacao       0
                                   :caminho nil))))
    (loop do
      (let ((no (pop fronteira)))
        (cond ((null no) (return nil))
              ((funcall solucao (no-estado no)) (return (reverse (no-caminho no))))
              (t
               ;; Expande o no e adiciona os nos resultantes a fronteira
               ;; ordenados pela funcao de avaliacao f(n) = g(n) + h(n).
               (let* ((estado (no-estado no))
                      (lista-accoes (funcall accoes estado)))
                 (dolist (accao lista-accoes)
                   (let* ((novo-estado (funcall resultado estado accao))
                          (novo-no (make-no :estado novo-estado
                                            :avaliacao (+ (funcall g novo-estado)
                                                          (funcall h novo-estado))
                                            :caminho (cons accao
                                                           (no-caminho no)))))
                     (setf fronteira
                           (merge 'list
                                  (list novo-no)
                                  fronteira
                                  #'no<)))))))))))

