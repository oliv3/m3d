-module(m3d).
-author('olivier@biniou.info').

%% API
-export([start/0]).

%% Internal exports
-export([m3d1/5]).

%% Macros
-define(N,   10).
-define(MAX, lists:seq(0, ?N-1)).


%% oliv3: relou l'ordre x/y/z ca oblige a faire
%% un lists:reverse on peut pas faire autrement ?
start() ->
    Refs = [m3d(X, Y, Z) || Z <- ?MAX,
			    Y <- ?MAX,
			    X <- ?MAX],
    Res = collect(Refs),
    io:format("Res= ~p~n", [Res]),
    Res.


m3d(X, Y, Z) ->
    %% io:format("~p ~p ~p~n", [X, Y, Z]),
    Ref = make_ref(),
    spawn(?MODULE, m3d1, [self(), Ref, X, Y, Z]),
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


m3d1(From, Ref, X, Y, Z) ->
    %% Lame mandelbulb function
    From ! {Ref, X * Y * Z}.
