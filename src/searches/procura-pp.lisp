;;; Versao com conjunto de estados ja explorados.
(defun procura-pp (problema)
  "procura-pp: problema --> lista de accoes"
  (let ((estados-explorados ())
        (estado-inicial (problema-estado-inicial problema))
        (solucao        (problema-solucao        problema))
        (accoes         (problema-accoes         problema))
        (resultado      (problema-resultado      problema)))
    (if (funcall solucao
                 estado-inicial)
        nil
        (labels
            ((procura-pp-recursivo (estado)
               (progn
                 (pushnew estado              ; Adiciona estado ao conjunto
                          estados-explorados) ; de estados explorados.
                 (let ((lista-accoes (reverse (funcall accoes
                                                       estado))))
                   (if (null lista-accoes)
                       (return-from procura-pp-recursivo
                         'INSUCESSO)
                       ;; Para cada accao possivel, por ordem LIFO, expande o
                       ;; estado e verifica se e solucao na geracao. Se for
                       ;; solucao devolve uma lista com a accao que o originou;
                       ;; senao, verifica se ja tinha sido explorado. Se sim,
                       ;; devolve insucesso; se nao, realiza uma procura a
                       ;; partir desse estado.
                       (dolist (accao lista-accoes)
                         (let ((novo-estado (funcall resultado
                                                     estado
                                                     accao)))
                           (when (funcall solucao
                                          novo-estado)
                             (return-from procura-pp-recursivo
                               (list accao)))
                           (when (not (member novo-estado
                                              estados-explorados
                                              :test #'estados-iguais-p))
                             (let ((procura (procura-pp-recursivo novo-estado)))
                               (when (not (equalp procura
                                                  'INSUCESSO))
                                 (return-from procura-pp-recursivo
                                   (cons accao procura)))))))))
                 'INSUCESSO)))
          (procura-pp-recursivo estado-inicial)))))

;;; Versao do procura-pp sem manter conjunto de estados ja explorados.
(defun procura-pp-2 (problema)
  "procura-pp: problema --> lista de accoes"
  (let ((estado-inicial (problema-estado-inicial problema))
        (solucao        (problema-solucao        problema))
        (accoes         (problema-accoes         problema))
        (resultado      (problema-resultado      problema)))
    (if (funcall solucao estado-inicial)
        nil
        (labels
            ((procura-pp-recursivo (estado)
               (let ((lista-accoes (reverse (funcall accoes estado))))
                 (if (null lista-accoes)
                     (return-from procura-pp-recursivo
                       'INSUCESSO)
                     ;; Para cada accao possivel, por ordem LIFO, expande o
                     ;; estado e verifica se e solucao na geracao. Se for
                     ;; solucao devolve uma lista com a accao que o originou;
                     ;; senao, verifica se ja tinha sido explorado. Se sim,
                     ;; devolve insucesso; se nao, realiza uma procura a
                     ;; partir desse estado.
                     (dolist (accao lista-accoes 'INSUCESSO)
                       (let ((novo-estado (funcall resultado
                                                   estado
                                                   accao)))
                         (when (funcall solucao novo-estado)
                           (return-from procura-pp-recursivo
                             (list accao)))
                         (let ((procura (procura-pp-recursivo novo-estado)))
                           (when (not (equalp procura 'INSUCESSO))
                             (return-from procura-pp-recursivo
                               (cons accao procura))))))))))
          (procura-pp-recursivo estado-inicial)))))

