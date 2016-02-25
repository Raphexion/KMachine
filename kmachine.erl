-module(kmachine).
-export([tr/1]).
-include_lib("eunit/include/eunit.hrl").

%% Background information
%% In lambda calculus we sometimes write
%% \x.x
%% If we use deBruijn notation (nameless)
%% we would write this as
%% \.#0
%% This can be compared to that
%% \x.y is written as \.#1
%%
%% If we want to evaluate
%% (\x.x \x.x)
%% it would be written
%% (\.#0 \.#0)
%% And the correct evaluation is
%% \.#
%% or
%% \x.x in the "standard" notation.

%% Krivine's machine (K-machine)
%% as described in the book
%% Abstract Computing Machines
%% by Kluge
%% page 105-106


%% rule number 1
tr({[{suspension, Env, Exp} | _E], {deBruijn, 0}, S}) ->
    tr({ Env, Exp, S });

%% rule number 2
tr({[ _V | Env ], {deBruijn, N}, S}) ->
    tr({ Env, {deBruijn, N-1}, S});

%% rule number 3
tr({Env, {application, E0, E1}, S}) ->
    tr({ Env, E0, [{suspension, Env, E1}] ++ S});

%% rule number 4
tr({Env, {abstraction, Eb}, [ H | S]}) ->
    tr({ [H] ++ Env, Eb, S});

%% stop of machine, provide result
tr({_E, R, []}) ->
    R.


%% Tests and example of the machine
tr_test_() ->
    NIL = [],

    T0 = {deBruijn, 0},
    T1 = {deBruijn, 1},

    T2a = {abstraction, T0},
    T2b = {application, T2a, T2a },

    T3a = {abstraction, T1},
    T3b = {application, T2a, T3a },

    [
     %% #0 -> #0
     ?_assert(tr({NIL, T0, NIL}) =:= T0),

     %% #1 -> #1
     ?_assert(tr({NIL, T1, NIL}) =:= T1),

     %% (\.#0 \.#0) -> \.#0
     ?_assert(tr({NIL, T2b, NIL}) =:= T2a),

     %% (\.#0 \.#1) -> \.#1
     ?_assert(tr({NIL, T3b, NIL}) =:= T3a)
    ].
