FINAL=parte_2.lisp
SRCS=tabuleiro.lisp estado.lisp accao.lisp problema.lisp

all: ${FINAL}

${FINAL}: ${SRCS}
	cat $^ > $@
	printf '\n(load "utils.lisp")\n' >> $@

test: ${FINAL}
	cat utils.lisp $^ > tmp && mv tmp $^
	clisp -c $^

clean:
	rm -f ${FINAL} *.fas *.lib

