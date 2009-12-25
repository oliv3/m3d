DEBUG=-DDEBUG

all: m3d.erl
	erlc +hipe -Wall m3d.erl