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
         (load "testes.lisp")))

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
