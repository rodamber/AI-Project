FINAL=2a-entrega.lisp
SRCS=tabuleiro.lisp estado.lisp accao.lisp problema.lisp \
tabuleiros-pre-computados.lisp \
searches/procura-pp.lisp searches/procura-astar.lisp \
searches/heuristicas.lisp searches/procura-best.lisp gen.lisp

all: ${FINAL}

${FINAL}: ${SRCS}
	cat $^ > $@
	printf '\n(load "utils.lisp")\n' >> $@
	clisp -c testes.lisp

test: ${FINAL}
	cat utils.lisp $^ > tmp && mv tmp $^
	clisp -c $^

clean:
	rm -f *.fas *.lib

