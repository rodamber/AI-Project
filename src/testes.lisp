(load (compile-file "tabuleiros-pre-computados.lisp"))
(load (compile-file "2a-entrega.lisp"))
(load (compile-file "algoritmo-genetico.lisp"))
(load (compile-file "gen.lisp"))
(load (compile-file "utils.lisp"))

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

(defun joga (estado accoes)
  (executa-jogadas estado accoes))

(defun print-array (arr)
  (executa-jogadas (make-estado :tabuleiro arr) ()))

(defun teste (procura arr pecas)
  (print "Teste a correr...")
  (let ((res (time (funcall procura arr pecas))))
    (executa-jogadas (make-estado :tabuleiro         arr
                                  :pecas-por-colocar pecas)
                     res)))

(defun doit ()
  (progn (run-shell-command "make clean all")
         (load (compile-file "testes.lisp"))))

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

(defun pre-computa-pecas (s n)
  (let ((pecas nil))
    (dotimes (i n)
      (push (random-pecas s) pecas))
    (make-array (list n) :initial-contents pecas)))

(setf pecas-pre-computadas (pre-computa-pecas 4 100))

(defun testes (procura n log-name)
  (let ((tabuleiros (subseq tabuleiros-pre-computados 0 n))
        (problema (make-problema))
        (log-file (concatenate 'string "../data/" log-name "-" (current-date-string) ".log")))
    (with-open-file (stream log-file :direction :output)
      (format stream "Teste Tempo Pontuacao~%")
      (dotimes (i (array-dimension tabuleiros 0))
        (setf (estado-tabuleiro (problema-estado-inicial problema))
              (aref tabuleiros i))
        (setf (estado-pecas-por-colocar (problema-estado-inicial problema))
              (aref pecas-pre-computadas i))
        (setf tempo (get-time (setf lista-de-accoes (funcall procura problema))))
        (setf pontos (pontuacao (problema-estado-inicial problema)
                                lista-de-accoes))
        (format stream "~D ~$ ~D~%" i tempo pontos)))))

(defun joga-testes (procura n)
  (let ((tabuleiros (subseq tabuleiros-pre-computados 0 n))
        (problema (make-problema))
        (tempo-total 0))
    (dotimes (i (array-dimension tabuleiros 0))
      (setf (estado-tabuleiro (problema-estado-inicial problema))
            (aref tabuleiros i))
      (setf (estado-pecas-por-colocar (problema-estado-inicial problema))
            (aref pecas-pre-computadas i))
      (incf tempo-total (get-time (setf lista-de-accoes (funcall procura problema))))
      (joga (problema-estado-inicial problema) lista-de-accoes))
    (/ tempo-total n)))

(defun procura-A*-temporizado (problema heuristica tempo-limite)
  (let* ((estado-inicial (problema-estado-inicial problema))
         (solucao        (problema-solucao        problema))
         (accoes         (problema-accoes         problema))
         (resultado      (problema-resultado      problema))
         (g              (problema-custo-caminho  problema))
         (h heuristica)
         (fronteira (list (make-no :estado          estado-inicial
                                   :avaliacao       0
                                   :caminho nil)))
         (relogio 0))
    (loop do
      (if (> relogio tempo-limite)
          (return nil)
          (incf relogio
                (get-time
                 (let ((no (pop fronteira)))
                   (cond ((null no) (return nil))
                         ((funcall solucao (no-estado no)) (return (reverse (no-caminho no))))
                         (t (let* ((estado (no-estado no))
                                   (lista-accoes (funcall accoes estado)))
                              (dolist (accao lista-accoes)
                                (let* ((novo-estado (funcall resultado estado accao))
                                       (novo-no (make-no :estado novo-estado
                                                         :avaliacao (+ (funcall g novo-estado)
                                                                       (funcall h novo-estado))
                                                         :caminho (cons accao
                                                                        (no-caminho no)))))
                                  (setf fronteira
                                        (merge 'list (list novo-no) fronteira #'no<))))))))))))))
