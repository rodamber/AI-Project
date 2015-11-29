(load "tabuleiro.lisp")
(load "estado.lisp")
(load "accao.lisp")
(load "problema.lisp")
(load "tabuleiros-pre-computados.lisp")
(load "utils.lisp")
(load "searches/procura-pp.lisp")
(load "searches/procura-astar.lisp")
(load "searches/procura-best.lisp")
(load "searches/heuristicas.lisp")

(defun problema-random (numero-de-pecas)
  (let ((estado (make-estado :pecas-por-colocar (random-pecas numero-de-pecas)
                             :tabuleiro         (cria-tabuleiro))))
    (make-problema :estado-inicial estado)))

;;;-----------------------------------------------------------------------------
;;; Profiling

(defun random-number-from-to (from to)
  (+ from (random (1+ (- to from)))))

(defun executa-accoes (estado accoes)
  (if (null accoes)
      estado
      (executa-accoes (resultado estado
                                 (car accoes))
                      (cdr accoes))))

(defun pontuacao (estado accoes)
  (estado-pontos (executa-accoes estado accoes)))

(defun heuristica-altura-3 (estado)
  "heuristica-altura-3: estado --> numero
Recompensa pela altura ser um multiplo de 3"
  (let* ((tabuleiro (estado-tabuleiro estado))
         (altura-max (apply #'max
                            (loop for c
                                  from 0 to 9
                                  collect (tabuleiro-altura-coluna tabuleiro
                                                                   c)))))
    (floor (/ altura-max
              3))))

(defun random-estado (pecas)
  (let* ((estado (make-estado :pecas-por-colocar pecas))
         (sol    (lambda (estado)
                   (and (solucao estado)
                        (zerop (estado-pontos estado)))))
         (avaliacao (lambda (estado)
                      (+ (custo-buracos estado)
                         (custo-relevo  estado)))))
    (labels
        ((procura-worst (array pecas f poda-periodo poda-tamanho)
           (let* ((tabuleiro      (array->tabuleiro array))
                  (estado-inicial (make-estado :tabuleiro tabuleiro
                                               :pecas-por-colocar pecas))
                  (fronteira      (list (make-no :estado    estado-inicial
                                                 :avaliacao 0
                                                 :caminho   nil))))
             (loop do
               (dotimes (i poda-periodo)
                 (let ((no (pop fronteira)))
                   (cond
                     ((null no)
                      (return-from procura-worst
                        nil))
                     ((funcall sol (no-estado no))
                      (return-from procura-worst
                        (reverse (no-caminho no))))
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
                                         #'no<)))))))))
               (setf fronteira
                     (poda fronteira
                           poda-tamanho))))))
      (let ((accoes (funcall #'procura-worst (estado-tabuleiro estado) pecas avaliacao 10 50)))
        (executa-accoes estado accoes)))))

(defun random-array ()
  (tabuleiro->array (estado-tabuleiro (random-estado (random-pecas 8)))))

(defun print-array (arr)
  (executa-jogadas (make-estado :tabuleiro arr) ()))

(defun compute-arrays (how-many)
  (make-array (list how-many)
              :initial-contents (loop for i from 1 to how-many
                                      collect (random-array))))

(defun teste-1 (arr pecas f poda-periodo poda-tamanho)
  (let ((res (time (procura-best-parametrizada arr
                                               pecas
                                               f
                                               poda-periodo
                                               poda-tamanho))))
    (executa-jogadas (make-estado :tabuleiro         arr
                                  :pecas-por-colocar pecas)
                     res)))

(defun teste-2 (arr pecas f poda-periodo poda-tamanho)
  (let ((res (get-time (procura-best-parametrizada arr
                                                   pecas
                                                   f
                                                   poda-periodo
                                                   poda-tamanho))))
    (list (car res)
          (pontuacao (make-estado :tabuleiro         arr
                                  :pecas-por-colocar pecas)
                     (cadr res)))))

;; (defun teste-valores-de-poda (numero-de-expansoes tamanho-da-seleccao)
;;   (labels
;;       ((f (e)
;;          (+ (custo-oportunidade e)
;;             (heuristica-pontuacao e))))
;;     (progn
;;       (print (list 'TESTE-VALORES-DE-PODA numero-de-expansoes tamanho-da-seleccao))
;;       (dotimes (i 3)
;;         (let ((tabuleiro (random-array)))
;;           (dotimes (j 3)
;;             (let ((pecas (random-pecas 6)))
;;               (print (teste-2 tabuleiro pecas #'f numero-de-expansoes tamanho-da-seleccao)))))))))


(defmacro get-time (&body forms)
  (let ((run1 (gensym))
        (run2 (gensym))
        (result (gensym)))
    `(let* ((,run1 (get-internal-run-time))
            (,result (progn ,@forms))
            (,run2 (get-internal-run-time)))
       (list (float (/ (- ,run2 ,run1) internal-time-units-per-second))
             ,result))))

(defun dlog (fn form)
  (dribble (concatenate 'string fn (current-date-string) ".log"))
  form
  (dribble))

(defun mean (&rest sequence)
  (if (null sequence)
      nil
      (/ (reduce #'+ sequence) (length sequence))))

(defun current-date-string ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" yr mon day hr min sec)))
