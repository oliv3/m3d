-module(m3d).
-author('olivier@biniou.info').

%% API
-export([start/0, start/1]).

%% Internal exports
-export([m3d1/3]).

%% Macros
-define(N,     10).
-define(MAX,   lists:seq(0, ?N-1)).
-define(FNAME, "test.df3"). %% default filename


-define(XMIN, -2.5).
-define(XMAX, +1.5).
-define(YMIN, -2.0).
-define(YMAX, +2.0).
-define(ZMIN, -2.0).
-define(ZMAX, +2.0).


start() ->
    start(?FNAME).
start(Filename) ->
    %% open file for writing
    case file:open(Filename, [write, binary]) of
	{ok, Fd} ->
	    main(Fd),
	    file:close(Fd);
	Error ->
	    exit(Error)
    end.


main(Fd) ->
    ok.


m3d(X, Y, Z) ->
    io:format("~p ~p ~p~n", [X, Y, Z]),
    Ref = make_ref(),
    spawn(?MODULE, m3d1, [self(), Ref, {X, Y, Z}]),
    Ref.


collect(Refs) ->
    collect(Refs, []).
collect([], Acc) ->
    lists:reverse(Acc);
collect([Ref|Refs], Acc) ->
    receive
	{Ref, Result} ->
	    collect(Refs, [Result|Acc])
    end.


m3d1(From, Ref, {X, Y, Z}) ->
    %% Lame mandelbulb function
    From ! {Ref, X * Y * Z}.
