-module(atom_gotchas_app).

-behaviour(application).

-export([start/2, stop/1]). % callbacks
-export([atom5/0]). % exposed function

%% "atom bomb" gotcha demo function
-export([generate_atoms/1]).

%% "attempt_atom" gotcha demo functions
-export([json/1,
         decode/1,
         check/1]).

%% This section, together with the `atom_bomb.escript` represents the solution
%% We create a new function (and export it, since it's not used internally)
-export([load_all_atoms/0]).

%% We include the `.hrl` file containing the `?ALL_ATOMS` macro
-include("all_atoms.hrl").

load_all_atoms() ->
    %% We use the `?ALL_ATOMS` macro (which is filled with all atom literals used
    %% in the current application, by running `atom_bomb.escript`)
    %% Thus, all atom literals are created when this module is loaded
    ?ALL_ATOMS.

%% The `application` callback functions
start(_StartType, _StartArgs) ->
    %% Starts a dummy supervisor
    atom_gotchas_sup:start_link().

stop(_State) ->
    %% The following is to prove that atom literals used in internal
    %% functions are created, regardless of when the functions are called
    atom6(),
    load_atom7().

%% The function to demonstrate the problem with dynamically generating atoms
-spec generate_atoms(ThousandsOfAtoms :: integer()) -> ok.
generate_atoms(ThousandsOfAtoms) ->
    %% Have a unique prefix, so that everytime we call this function, we get new atoms
    UniquePart = integer_to_list(os:system_time()),
    %% Generate and decode `ThousandsOfAtoms` maps, each with 1K unique keys
    lists:foreach(
      fun(Index) ->
              Json = json(UniquePart, Index * 1000, 1000),
              decode(Json, [{keys, atom}])
      end, lists:seq(1, ThousandsOfAtoms)),
    io:format("~w atoms generated~n", [ThousandsOfAtoms * 1000]).


%% The functions used to demonstrate the problem posed by decoding with `attempt_atom`
-spec json(NumberOfKeys :: integer()) -> binary().
json(NumberOfKeys) ->
    json("atom", 1, NumberOfKeys).

-spec json(KeyPrefix :: string(),
           First :: integer(),
           NumberOfKeys :: integer()) -> binary().
json(KeyPrefix, First, NumberOfKeys) ->
    %% Create a test encoded Json object
    %% the keys of the Json have the form `atom<integer>`
    Json = lists:foldl(
             fun(N, Acc) ->
                     Key = list_to_binary(KeyPrefix ++ integer_to_list(N)),
                     Acc#{ Key => N }
             end, #{}, lists:seq(First, First + NumberOfKeys - 1)),
    jsone:encode(Json).

-spec decode(binary()) -> map().
decode(Encoded) ->
    %% Decode the input with `attempt_atom` option
    decode(Encoded, [{keys, attempt_atom}]).

decode(Encoded, Opts) ->
    jsone:decode(Encoded, Opts).

-spec check(map()) -> ok.
check(Decoded) ->
    %% Check how many `atom<N>` keys were decoded to atoms
    NrKeys = length(maps:keys(Decoded)),
    lists:foreach(
      fun(N) ->
              case internal:atom_exists(N, Decoded) of
                  true ->
                      io:format("atom~w found~n", [N]),
                      ok;
                  false ->
                      io:format("atom~w NOT FOUND!!!~n", [N])
              end
      end, lists:seq(1, NrKeys)).

%% internal functions

%% Being the name of a function, literal `atom5` exists
%% as soon as the module is loaded
atom5() ->
    ok.

%% Being the name of a function, literal `atom6` exists
%% as soon as the module is loaded
atom6() ->
    ok.

%% Being used as a literal here (even if as part of a macro)
%% `atom7` exists as soon as the module is loaded
-define(ATOMS, [atom7]).
load_atom7() ->
    ?ATOMS.
