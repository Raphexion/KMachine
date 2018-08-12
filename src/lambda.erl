-module(lambda).

-export([debruijn/1,
	 identity/0,
	 const/1,
	 abstraction/1,
	 application/2]).

debruijn(N) ->
    {deBruijn, N}.

identity() ->
    {abstraction, debruijn(0)}.

const(N) when N /= 0 ->
    {abstraction, debruijn(N)}.

abstraction(Body) ->
    {abstraction, Body}.

application(Operator, Operand) ->
    {application, Operator, Operand}.
