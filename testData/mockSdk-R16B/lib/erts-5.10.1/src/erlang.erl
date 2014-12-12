-module(erlang).

-export([crc32/1, crc32/2, erase/0, erase/1, abs/1, dt_get_tag/0, atom_to_binary/2]).

%% crc32/1
-spec erlang:crc32(Data) -> non_neg_integer() when
      Data :: iodata().
crc32(_Data) ->
    erlang:nif_error(undefined).

%% erase/1
-spec erase(Key) -> Val | undefined when
      Key :: term(),
      Val :: term().
erase(_Key) ->
    erlang:nif_error(undefined).

is_record(_Term,_RecordTag) ->
    erlang:nif_error(undefined).

is_record(_Term,_RecordTag,_Size) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:abs/1
-spec abs(Float) -> float() when
      Float :: float();
         (Int) -> non_neg_integer() when
      Int :: integer().
abs(_Number) ->
    erlang:nif_error(undefined).

%% atom_to_binary/2
-spec atom_to_binary(Atom, Encoding) -> binary() when
      Atom :: atom(),
      Encoding :: latin1 | unicode | utf8.
atom_to_binary(_Atom, _Encoding) ->
    erlang:nif_error(undefined).

%% dt_get_tag/0
-spec erlang:dt_get_tag() -> binary() | undefined.
dt_get_tag() ->
    erlang:nif_error(undefined).