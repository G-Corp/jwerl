-module(jwerl).

-export([sign/1, sign/2,
         verify/1, verify/2,
         payload/1, header/1]).

-define(DEFAULT_ALG, <<"HS256">>).
-define(DEFAULT_HEADER, #{typ => <<"JWT">>,
                          alg => ?DEFAULT_ALG}).

sign(Data) ->
  sign(Data, #{}).
sign(Data, Options) ->
  encode(jsx:encode(Data), config_headers(Options), maps:get(key, Options, <<>>)).

verify(Data) ->
  verify(Data, #{}).
verify(Data, Options) ->
  CheckClaims = maps:get(check_claims, Options, true),
  case decode(Data,
              algorithm_to_atom(maps:get(alg, Options, ?DEFAULT_ALG)),
              maps:get(key, Options, <<>>)) of
    {ok, TokenData} when CheckClaims ->
      case (catch check_claims(TokenData)) of
        ok ->
          {ok, TokenData};
        Reason ->
          {error, Reason}
      end;
    Result ->
      Result
  end.

payload(Data) ->
  {ok, P} = payload(Data, none, none),
  P.

header(Data) ->
  decode_header(Data).

check_claims(TokenData) ->
  Now = os:system_time(seconds),
  check_claim(TokenData, exp, fun(ExpireTime) ->
                    Now < ExpireTime
                end, expired),
  check_claim(TokenData, iat, fun(IssuedAt) ->
                    IssuedAt =< Now
                end, future_issued_at),
  check_claim(TokenData, nbf, fun(NotBefore) ->
                    NotBefore =< Now
                end, not_yet_valid),
  ok.

check_claim(TokenData, Key, F, FailReason) ->
  case maps:find(Key, TokenData) of
    error ->
      %% Ignore if missing. If it has been correctly signed,
      %% this was intended.
      true;
    {ok, Value} ->
      %% Call back if found for custom checking logic
      case F(Value) of
        true -> ok;
        false -> throw(FailReason)
      end
  end.

encode(Data, #{alg := none} = Options, _) ->
  encode_input(Data, Options);
encode(Data, Options, Key) ->
  Input = encode_input(Data, Options),
  <<Input/binary, ".", (signature(maps:get(alg, Options), Key, Input))/binary>>.

decode(Data, Alg, Key) ->
  Header = decode_header(Data),
  case algorithm_to_atom(maps:get(alg, Header)) of
    Alg -> payload(Data, Alg, Key);
    Alg1 -> {error, invalid_algorithm, Alg1, Alg}
  end.

base64_encode(Data) ->
  Data1 = base64_encode_strip(lists:reverse(base64:encode_to_string(Data))),
  << << (urlencode_digit(D)) >> || <<D>> <= Data1 >>.
base64_encode_strip([$=|Rest]) ->
  base64_encode_strip(Rest);
base64_encode_strip(Result) ->
  list_to_binary(lists:reverse(Result)).

base64_decode(Data) ->
  Data1 = << << (urldecode_digit(D)) >> || <<D>> <= Data >>,
  Data2 = case byte_size(Data1) rem 4 of
            2 -> <<Data1/binary, "==">>;
            3 -> <<Data1/binary, "=">>;
            _ -> Data1
          end,
  base64:decode(Data2).

urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.

urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D)  -> D.

config_headers(Options) ->
  maps:merge(?DEFAULT_HEADER, Options).

decode_header(Data) ->
  [Header|_] = binary:split(Data, <<".">>, [global]),
  jsx:decode(base64_decode(Header), [return_maps, {labels, atom}]).

payload(Data, none, _) ->
  [_, Data1|_] = binary:split(Data, <<".">>, [global]),
  {ok, jsx:decode(base64_decode(Data1), [return_maps, {labels, atom}])};
payload(Data, Alg, Key) ->
  [Header, Data1, Signature] = binary:split(Data, <<".">>, [global]),
  {AlgMod, ShaBits} = algorithm_to_infos(Alg),
  case erlang:apply(AlgMod, verify, [ShaBits,
                                     Key,
                                     <<Header/binary, ".", Data1/binary>>,
                                     base64_decode(Signature)]) of
    true ->
      {ok, jsx:decode(base64_decode(Data1), [return_maps, {labels, atom}])};
    _ ->
      {error, invalid_signature}
  end.

encode_input(Data, Options) ->
  <<(base64_encode(jsx:encode(Options)))/binary, ".", (base64_encode(Data))/binary>>.

signature(Alg, Key, Data) ->
  {AlgMod, ShaBits} = algorithm_to_infos(Alg),
  Signature = erlang:apply(AlgMod, sign, [ShaBits, Key, Data]),
  base64_encode(Signature).

algorithm_to_atom(<<"HS256">>) -> hs256;
algorithm_to_atom(<<"RS256">>) -> rs256;
algorithm_to_atom(<<"ES256">>) -> es256;
algorithm_to_atom(<<"HS386">>) -> hs386;
algorithm_to_atom(<<"RS386">>) -> rs386;
algorithm_to_atom(<<"ES386">>) -> es386;
algorithm_to_atom(<<"HS512">>) -> hs512;
algorithm_to_atom(<<"RS512">>) -> rs512;
algorithm_to_atom(<<"ES512">>) -> es512;
algorithm_to_atom(A) when is_atom(A) -> A;
algorithm_to_atom(_) -> none.

algorithm_to_binary(hs256) -> <<"HS256">>;
algorithm_to_binary(rs256) -> <<"RS256">>;
algorithm_to_binary(es256) -> <<"ES256">>;
algorithm_to_binary(hs386) -> <<"HS386">>;
algorithm_to_binary(rs386) -> <<"RS386">>;
algorithm_to_binary(es386) -> <<"ES386">>;
algorithm_to_binary(hs512) -> <<"HS512">>;
algorithm_to_binary(rs512) -> <<"RS512">>;
algorithm_to_binary(es512) -> <<"ES512">>;
algorithm_to_binary(A) when is_binary(A) -> A;
algorithm_to_binary(_) -> <<"none">>.

algorithm_to_infos(Algo) ->
  case algorithm_to_binary(Algo) of
    <<"HS", ShaBits/binary>> ->
      {jwerl_hs, binary_to_integer(ShaBits)};
    <<"RS", ShaBits/binary>> ->
      {jwerl_rs, binary_to_integer(ShaBits)};
    <<"ES", ShaBits/binary>> ->
      {jwerl_es, binary_to_integer(ShaBits)};
    _ ->
      exit(invalid_algorithme)
  end.
