FINAL=parte_1.lisp
SRCS=tabuleiro.lisp estado.lisp accao.lisp problema.lisp

all: ${FINAL}

${FINAL}: ${SRCS}
	cat $^ > $@

test: ${FINAL}
	cat utils.lisp $^ > tmp && mv tmp $^
	clisp -c $^

clean:
	rm -f ${FINAL} *.fas *.lib

mooshak: ${FINAL}
	printf '\n(load "utils.fas")\n' >> $^

