-module(internal).

-export([atom_exists/2]).
-include("internal.hrl").

atom_exists(1, Map) ->
    atom1_exists(Map);
atom_exists(2, Map) ->
    atom2_exists(Map);
atom_exists(3, Map) ->
    atom3_exists(Map);
atom_exists(4, Map) ->
    atom4_exists(Map);
atom_exists(N, Map) ->
    Key = list_to_atom("atom" ++ integer_to_list(N)),
    maps:is_key(Key, Map).

atom1_exists(#{ atom1 := _ }) ->
    true;
atom1_exists(_) ->
    false.

atom2_exists(Map) ->
    case Map  of
        #{ atom2 := _ } ->
            true;
        _ ->
            false
    end.

atom3_exists(Map) ->
    case maps:get(atom3, Map, undefined) of
        undefined ->
            false;
        _ ->
            true
    end.

atom4_exists(Map) ->
    maps:is_key(?ATOM4, Map).
