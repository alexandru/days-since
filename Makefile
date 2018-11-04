.DEFAULT_GOAL := compile

compile:
	stack build

install: compile
	stack install
	mv "`stack path | grep local-bin-path | awk '{print $$2}'`/days-since" /usr/local/bin
