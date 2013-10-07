ERLOPTS= -pa . -DTEST
ERLC=erlc $(ERLOPTS)

all: deck.beam bids.beam game_fsm.beam game_sup.beam player.beam player_sup.beam

clean:
	rm -f *.beam

player.beam: player.erl
	$(ERLC) player.erl

player_sup.beam: player_sup.erl
	$(ERLC) player_sup.erl

deck.beam: deck.erl
	$(ERLC) deck.erl

game_fsm.beam: game_fsm.erl
	$(ERLC) game_fsm.erl

game_sup.beam: game_sup.erl
	$(ERLC) game_sup.erl

bids.beam: bids.erl
	$(ERLC) bids.erl
