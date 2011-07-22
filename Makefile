ERLC:=erlc
ERL:=erl
REBAR:=rebar

all: mochijson2 dispatcher beams

mochijson2: src/mochijson2.erl src/mochinum.erl
	$(ERLC) -o ebin/ src/mochijson2.erl
	$(ERLC) -o ebin/ src/mochinum.erl

beams:
	$(REBAR) compile

dispatcher: tools/dispatcher_generator.erl src/Inspector.json
	$(ERLC) -o tools/ $<
	$(ERL) -noshell -pa ebin -pa tools -eval "dispatcher_generator:generate(\"src/Inspector.json\"), halt()."

clean:
	rm -f src/*.inc
	$(REBAR) clean
