FINAL=2a-entrega.lisp
SRCS=tabuleiro.lisp estado.lisp accao.lisp problema.lisp \
searches/heuristicas.lisp searches/procura-pp.lisp searches/procura-astar.lisp \
searches/procura-best.lisp

all: ${FINAL}

${FINAL}: ${SRCS}
	cat $^ > $@
	printf '\n(load "utils.fas")\n' >> $@

test: ${FINAL}
	cat utils.lisp $^ > tmp && mv tmp $^
	clisp -c $^

clean:
	rm -f ${FINAL} *.fas *.lib

