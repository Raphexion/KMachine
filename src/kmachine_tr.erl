-module(kmachine_tr).
-export([tr/1]).
-include_lib("eunit/include/eunit.hrl").

%% This module only performs one transfer.
%% The rationele is
%% we want maximum testability

%% Krivine's machine (K-machine)
%% as described in the book
%% Abstract Computing Machines
%% by Kluge
%% page 105-106

%% rule number 1
tr({[{suspension, Env, Exp} | _E], {deBruijn, 0}, S}) ->
    { Env, Exp, S };

%% rule number 2
tr({[ _V | Env ], {deBruijn, N}, S}) ->
    { Env, {deBruijn, N-1}, S};

%% rule number 3
tr({Env, {application, E0, E1}, S}) ->
    { Env, E0, [{suspension, Env, E1}] ++ S};

%% rule number 4
tr({Env, {abstraction, Eb}, [ H | S]}) ->
    {[H] ++ Env, Eb, S};

%% nothing more can be down.
tr({_E, R, []}) ->
    {stop, R}.

%%
%% TESTS
%%

-define(NIL, []).
-define(V0, {deBruijn, 0}).
-define(V1, {deBruijn, 1}).

stop_test_() ->
    [ ?_assert(tr({?NIL, ?V0 , ?NIL}) =:= {stop, ?V0}),
      ?_assert(tr({?NIL, ?V1 , ?NIL}) =:= {stop, ?V1}) ].

-define(A0, {application, ?V0, ?V1}).
-define(S0, {suspension, ?NIL, ?V1}).

rule3_test_() ->
    In = {?NIL, ?A0, ?NIL},
    Out = tr(In),
    Expected = {?NIL, ?V0, [?S0]},
    ?_assert(Out =:= Expected).

rule1_test_() ->
    In = {[{suspension, ?NIL, ?V1}], ?V0, ?NIL},
    Out = tr(In),
    Expected = {?NIL, ?V1, ?NIL},
    ?_assert(Out =:= Expected).

rule2_test_() ->
    In = {[env1, env0], ?V1, ?NIL},
    Out = tr(In),
    Expected = {[env0], ?V0, ?NIL},
    ?_assert(Out =:= Expected).

-define(B0, {abstraction, ?V1}).
rule4_test_() ->
    In = {?NIL, ?B0, [marker1, marker0]},
    Out = tr(In),
    Expected = {[marker1], ?V1, [marker0]},
    ?_assert(Out =:= Expected).
