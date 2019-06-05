REBAR=rebar3

compile:
	${REBAR} compile

clean:
	${REBAR} clean

bench:
	erl -pa ebin -pa _build/default/lib/*/ebin -noshell -config test/sys.config -eval "benchmark:bench(600000, 600)." -eval "init:stop()."

