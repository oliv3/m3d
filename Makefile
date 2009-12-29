# DEBUG=-DDEBUG
CLOUD=-Dcloud

all: m3d.erl
	erlc +hipe -Wall $(DEBUG) $(CLOUD) m3d.erl

clean:
	@rm -f erl_crash.dump *.beam *~
