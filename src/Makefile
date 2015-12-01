FINAL=2a-entrega.lisp
SRCS=tabuleiro.lisp estado.lisp accao.lisp problema.lisp \
searches/procura-pp.lisp searches/procura-astar.lisp \
searches/heuristicas.lisp searches/procura-best.lisp gen.lisp

all: ${FINAL}

${FINAL}: ${SRCS}
	cat $^ > $@
	printf '\n(load "utils.fas")\n' >> $@
	clisp -c testes.lisp
	clisp -c utils.lisp

test: ${FINAL}
	cat utils.lisp $^ > tmp && mv tmp $^
	clisp -c $^

clean:
	rm -f *.fas *.lib

