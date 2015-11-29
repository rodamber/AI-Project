(compile-file "2a-entrega.lisp")
(compile-file "tabuleiros-pre-computados.lisp")
(compile-file "algoritmo-genetico.lisp")
(load "2a-entrega.fas")
(load "tabuleiros-pre-computados.fas")
(load "algoritmo-genetico.fas")

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

(defun doit (procura tab pecas)
  (progn (run-shell-command "make clean all")
         (load "testes.lisp")
         (teste procura
                (aref tabuleiros-pre-computados tab)
                pecas)))


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
