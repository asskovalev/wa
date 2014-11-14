app:
	@./rebar skip_deps=true compile

all:
	./rebar get-deps compile

clean:
	@./rebar clean
	@rm -f erl_crash.dump

run:
	ERL_LIBS=..:deps erl -name wa@127.0.0.1 -pa ebin -s wa

test:
	mkdir -p logs
	ct_run -pa ebin -pa deps/*/ebin -logdir logs/ -dir test/

.PHONY: test

