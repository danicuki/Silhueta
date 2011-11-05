# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -noshel start_clean

MODS = silhueta matrix

all: compile

compile: ${MODS:%=%.beam}

## run an application from the makefile

run: compile
	${ERL} -s silhueta main 1 entradas/entrada.txt saida.txt imagem.pgm

test: compile
	${ERL} -s silhueta test -s init stop

# remove all the code
clean:
	rm -rf *.beam erl_crash.dump *.txt *.pgm
