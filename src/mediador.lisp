(defun mediador (algoritmo heuristica id &optional (nome-ficheiro-out #P"mediador-out.txt"))
    "mediador: algoritmo x heuristica x id [x nome-ficheiro-out]
faz 100 testes a um certo algoritmo com uma certa heuristica, imprimindo o resultado dos testes
e a media de todos os testes para o ficheiro 'mediador-out.txt', caso nao seja definido outro
nome"
    (dribble nome-ficheiro-out)
    (let ((total 0))
        (dotimes (i 100)
            (let* ((tabuleiro (aref tabuleiros-pre-computados i))
                   (lista-pecas (aref pecas-pre-computadas i))
                   (estado (make-estado :tabuleiro tabuleiro
                                        :pecas-por-colocar lista-pecas))
                   (pontuacao (pontuacao estado (funcall algoritmo 
                                                         tabuleiro
                                                         lista-pecas
                                                         heuristica))))

                (print (format nil "       ~A RESULTADO: ~D" id pontuacao))
                (incf total pontuacao)))
        (print (format nil "FINAL: ~A RESULTADO: ~D" id (/ total 100))))
    (dribble))
