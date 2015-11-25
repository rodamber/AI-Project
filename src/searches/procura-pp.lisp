(defun procura-pp (problema)
  "procura-pp: problema --> lista de accoes"
  (let ((estado-inicial (problema-estado-inicial problema))
        (solucao        (problema-solucao        problema))
        (accoes         (problema-accoes         problema))
        (resultado      (problema-resultado      problema)))
    (labels
        ((procura-pp-recursivo (estado)
           "procura-pp-recursivo: estado --> lista de accoes"
           (let ((lista-accoes (reverse (funcall accoes estado))))
             (if (null lista-accoes)
                 (return-from procura-pp-recursivo
                   nil)
                 ;; Para cada accao possivel, por ordem LIFO, expande o
                 ;; estado e verifica se e solucao na geracao. Se for
                 ;; solucao devolve uma lista com a accao que a originou; se
                 ;; nao, comeca uma procura a partir desse estado.
                 (dolist (accao lista-accoes nil)
                   (let ((novo-estado (funcall resultado estado accao)))
                     (when (funcall solucao novo-estado)
                       (return-from procura-pp-recursivo
                         (list accao)))
                     (let ((procura (procura-pp-recursivo novo-estado)))
                       ;; Se encontrarmos solucao a partir do estado gerado,
                       ;; juntamos a lista de accoes *devolvida* pela procura a
                       ;; accao que gerou esse estado. Se o resultado da procura
                       ;; for vazio, entao a procura nao encontrou solucao e
                       ;; procede-se a geracao do estado resultante da aplicacao
                       ;; da proxima accao na *lista-accoes*.
                       (when (not (null procura))
                         (return-from procura-pp-recursivo
                           (cons accao procura))))))))))
      (procura-pp-recursivo estado-inicial))))

