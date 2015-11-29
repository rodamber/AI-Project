(defun seleccao (fronteira n)
  (if (>= n (list-length fronteira))
      fronteira
      (subseq fronteira 0 n)))

(defun melhor-no (no-1 no-2)
  (cond ((null no-1) no-2)
        ((null no-2) no-1)
        (t
         (if (> (estado-pontos (no-estado no-1))
                (estado-pontos (no-estado no-2)))
             no-1
             no-2))))

(defmacro get-time (&body forms)
  (let ((run1   (gensym))
        (run2   (gensym))
        (result (gensym)))
    `(let* ((,run1   (get-internal-run-time))
            (,result (progn ,@forms))
            (,run2   (get-internal-run-time)))
       (float (/ (- ,run2
                    ,run1)
                 internal-time-units-per-second)))))

(defun funcao-avaliacao (estado)
  (funcao-avaliacao-parametrizada estado
                                  (list 1 #'custo-buracos)
                                  (list 1 #'custo-oportunidade)
                                  (list 1 #'heuristica-pontuacao-1)))

(defun funcao-avaliacao-parametrizada (estado &rest constantes-heuristicas)
  (apply #'+
         (map 'list
              #'(lambda (c-h)
                  (* (car c-h)
                     (funcall (cadr c-h)
                              estado)))
              constantes-heuristicas)))

(defun procura-best (array pecas)
  (procura-best-parametrizada array
                              pecas
                              #'funcao-avaliacao
                              1
                              8
                              19.9))

(defun procura-best-parametrizada (array
                                   pecas
                                   f
                                   seleccao-periodo
                                   seleccao-tamanho
                                   tempo-limite)
  (let* ((relogio      0)
         (no-objectivo nil)
         (tabuleiro         (array->tabuleiro array))
         (estado-inicial    (make-estado :tabuleiro tabuleiro
                                         :pecas-por-colocar pecas))
         (fronteira-inicial (list (make-no :estado    estado-inicial
                                           :avaliacao 0
                                           :caminho   nil)))
         (fronteira fronteira-inicial))
    (loop do
      (dotimes (i seleccao-periodo)
        (incf relogio
              (get-time
               (let ((no (pop fronteira)))
                 (cond
                   ;; Se for encontrado um beco sem saida recomeca com valores
                   ;; de seleccao mais abragentes.
                   ((null no)
                    (setf seleccao-periodo (* 2 seleccao-periodo))
                    (setf seleccao-tamanho (* 2 seleccao-tamanho))
                    (setf fronteira fronteira-inicial))
                   ;; Toma a melhor solucao encontrada ate entao e recomeca com
                   ;; valores de seleccao mais abrangentes.
                   ((solucao (no-estado no))

                    (print (format nil
                                   "no: ~4,'0d; periodo: ~4,'0d; tamanho: ~4,'0d"
                                   (estado-pontos (no-estado no))
                                   seleccao-periodo
                                   seleccao-tamanho))

                    (setf no-objectivo (melhor-no no no-objectivo))

                    (setf seleccao-periodo (* 2 seleccao-periodo))
                    (setf seleccao-tamanho (* 2 seleccao-tamanho))
                    (setf fronteira fronteira-inicial))
                   (t
                    ;; Expande o no e adiciona os nos resultantes a fronteira
                    ;; ordenados pela funcao de avaliacao f(n) = g(n) + h(n).
                    (let* ((estado       (no-estado no))
                           (lista-accoes (accoes estado)))
                      (dolist (accao lista-accoes)
                        (let* ((novo-estado (resultado estado accao))
                               (novo-no (make-no :estado novo-estado
                                                 :avaliacao (funcall f novo-estado)
                                                 :caminho (cons accao
                                                                (no-caminho no)))))
                          ;; Insere o no na fronteira.
                          (setf fronteira
                                (merge 'list
                                       (list novo-no)
                                       fronteira
                                       #'no<))))))))))
        ;; Se o tempo limite de execucao for ultrapassado, devolve-se a melhor
        ;; solucao encontrada ate entao.
        (if (> relogio tempo-limite)
            (return-from procura-best-parametrizada
              (if no-objectivo
                  (reverse (no-caminho no-objectivo))))))
      (setf fronteira
            (seleccao fronteira
                      seleccao-tamanho)))))


