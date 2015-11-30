(defun algoritmo-genetico (numero-de-testes
                           tamanho-da-populacao
                           tempo-por-teste
                           &rest heuristicas)
  (let* ((tamanho-dos-individuos (list-length heuristicas))
         (populacao (cria-populacao tamanho-da-populacao tamanho-dos-individuos))
         (geracao 1)
         (log-completo (concatenate 'string "../data/gen-" (current-date-string) "-complete.log"))
         (log-sumario  (concatenate 'string "../data/gen-" (current-date-string) "-summary.log")))

    (with-open-file (stream-complete  log-completo :direction :output)
      (with-open-file (stream-summary  log-sumario :direction :output)

        (format stream-complete "Individuo Resultado Parametros~%")
        (format stream-summary  "Geracao Media Best Parametros~%")

        (loop do
          (let* ((nova-populacao nil)
                 (tabuleiros (obtem-tabuleiros numero-de-testes))
                 (pecas      (obtem-pecas      numero-de-testes))
                 (resultados-da-geracao
                   (apply #'pre-processa-para-seleccao
                          stream-complete
                          populacao
                          numero-de-testes
                          tempo-por-teste
                          tabuleiros
                          pecas
                          heuristicas)))

            (format stream-summary
                    "~2,'0d ~D ~3,'0d ~{~$~^ ~}~%"
                    geracao
                    (car   resultados-da-geracao)  ; Media       
                    (cadr  resultados-da-geracao)  ; Best
                    (caddr resultados-da-geracao)) ; Parametros

            (princ (format nil
                           "=== Fim da Geracao ~2,'0d~T|~TMedia: ~3,'0d~T|~TBest: ~D~T|~TParametros: ~{~$~^ ~} ===~%"
                           geracao
                           (car   resultados-da-geracao)   ; Media
                           (cadr  resultados-da-geracao)   ; Best
                           (caddr resultados-da-geracao))) ; Parametros

            (dotimes (s (/ tamanho-da-populacao 2))
              (let* ((mae (seleccao-natural populacao))
                     (pai (seleccao-natural populacao))
                     (filhos (reproducao mae pai tamanho-dos-individuos))
                     (probabilidade-de-mutacao 0.05))
                (dolist (filho filhos)
                  (when (<= (probabilidade) probabilidade-de-mutacao)
                    (setf filho (mutacao filho)))
                  (setf (individuo-parametros filho)
                        (normaliza (individuo-parametros filho)))
                  (push filho nova-populacao))))
            (setf populacao nova-populacao))
          (incf geracao))))))

(defun cria-individuo-aleatorio (n)
  (make-individuo
   :parametros (normaliza (loop for i from 1 to n
                                collect (probabilidade)))))

(defun cria-populacao (tamanho-da-populacao n)
  (let ((populacao nil))
    (dotimes (tam tamanho-da-populacao populacao)
      (push (cria-individuo-aleatorio n) populacao))))

(defstruct individuo parametros (fitness 0))

(defun individuo> (i-1 i-2)
  (cond ((null i-1) i-2)
        ((null i-2) i-1)
        (t (> (individuo-fitness i-1)
              (individuo-fitness i-2)))))

(defun mutacao (individuo)
  (let* ((parametros (individuo-parametros individuo))
         (tamanho    (list-length parametros))
         (aleatorio  (random tamanho))
         (sinal      (potencia -1 (random 2))))
    (incf (nth aleatorio parametros)
          (* sinal 0.2))
    individuo))

(defun obtem-pecas (n)
  (loop for i from 1 to n
        collect (random-pecas 6)))

(defun obtem-tabuleiros (n)
  (loop for i from 1 to n
        collect (aref tabuleiros-pre-computados (random 1000))))

(defun pre-processa-para-seleccao (stream
                                   populacao
                                   numero-de-testes
                                   tempo-por-teste
                                   tabuleiros
                                   pecas
                                   &rest heuristicas)
  (let ((fitness-acumulado short-float-epsilon)
        (individuo-n 0)
        (media 0)
        (melhor 0)
        (constantes '(0 0 0)))

    (dolist (i populacao)
      (incf individuo-n)
      (let ((valor-fitness (apply #'tetris-fitness
                                  i
                                  numero-de-testes
                                  tempo-por-teste
                                  tabuleiros
                                  pecas
                                  heuristicas)))
        (when (> valor-fitness melhor)
          (setf melhor valor-fitness)
          (setf constantes (individuo-parametros i)))

        (incf fitness-acumulado valor-fitness)
        (setf media (/ fitness-acumulado individuo-n))

        (format stream
                "~D ~D ~{~$~^ ~}~%"
                individuo-n
                valor-fitness
                (individuo-parametros i))

        (princ (format nil
                       "~TIndividuo: ~2,'0d~T|~TResultado: ~3,'0d~T|~TParametros: ~{~$~^ ~}~%"
                       individuo-n
                       valor-fitness
                       (individuo-parametros i)))))

    (dolist (i populacao)
      (setf (individuo-fitness i) (/ (individuo-fitness i)
                                     fitness-acumulado)))

    (sort populacao #'individuo>)
    (setf fitness-acumulado 0)

    (dolist (i populacao)
      (incf fitness-acumulado (individuo-fitness i))
      (setf (individuo-fitness i) fitness-acumulado))

    (list media melhor constantes)))

(defun probabilidade ()
  (/ (random 1000) 1000))

(defun reproducao (mae pai tamanho-dos-individuos)
  (let* ((locus (random-number-from-to 1 (1- tamanho-dos-individuos)))
         (divide-mae (divide-lista (individuo-parametros mae) locus))
         (divide-pai (divide-lista (individuo-parametros pai) locus))
         (filho-1 (make-individuo :parametros (concatenate 'list
                                                           (car  divide-mae)
                                                           (cadr divide-pai))))
         (filho-2 (make-individuo :parametros (concatenate 'list
                                                           (car  divide-pai)
                                                           (cadr divide-mae)))))
    (list filho-1 filho-2)))

(defun seleccao-natural (populacao)
  (let ((r (probabilidade)))
    (dolist (i populacao)
      (when (> (individuo-fitness i) r)
        (return i)))))

(defun tetris-fitness (individuo numero-de-testes tempo-por-teste tabuleiros pecas &rest heuristicas)
  (let* ((parametros (individuo-parametros individuo))
         (constantes-heuristicas (mapcar #'list parametros heuristicas))
         (f (lambda (e) (apply #'funcao-avaliacao-parametrizada
                               e
                               constantes-heuristicas)))
         (resultados nil))
    (dotimes (n numero-de-testes)
      (let* ((tabuleiro (pop tabuleiros))
             (pecas-por-colocar (pop pecas))
             (accoes (procura-best-parametrizada tabuleiro
                                                 pecas-por-colocar
                                                 f
                                                 1
                                                 8
                                                 tempo-por-teste))
             (estado (make-estado :tabuleiro tabuleiro
                                  :pecas-por-colocar pecas-por-colocar)))
        (push (pontuacao estado accoes) resultados)))
    (setf (individuo-fitness individuo) (apply #'mean resultados))))

(defun norma (vector)
  (sqrt (apply #'+
               (map 'list
                    #'(lambda (x) (* x x))
                    vector))))

(defun normaliza (vector)
  (let ((n (norma vector)))
    (map 'list
         #'(lambda (x) (/ x n))
         vector)))

(defun potencia (x n)
  (if (zerop n)
      1
      (let ((res 1))
        (dotimes (i n res)
          (setf res (* res x))))))

(defun divide-lista (lista ponto)
  (list (subseq lista 0 ponto)
        (subseq lista ponto)))
