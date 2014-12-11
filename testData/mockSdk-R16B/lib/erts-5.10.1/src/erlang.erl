-module(erlang).

-export([crc32/1, crc32/2, erase/0, erase/1]).

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