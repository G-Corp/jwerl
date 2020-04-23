-module(jwerl).

-export([sign/1, sign/2, sign/3,
         verify/1, verify/2, verify/3, verify/4, verify/5,
         header/1]).

-on_load(conveniece_keys/0).

-define(DEFAULT_ALG, <<"HS256">>).
-define(DEFAULT_HEADER, #{typ => <<"JWT">>,
                          alg => ?DEFAULT_ALG}).

-type algorithm() :: hs256 | hs384 | hs512 |
                     rs256 | rs384 | rs512 |
                     es256 | es384 | es512 |
                     none.

% @equiv sign(Data, hs256, <<"">>)
-spec sign(Data :: map()) -> binary().
sign(Data) ->
    sign(Data, hs256, <<"">>).
% @equiv sign(Data, Algorithm, <<"">>)
-spec sign(Data :: map(), Algorithm :: algorithm()) -> binary().
sign(Data, Algorithm) ->
    sign(Data, Algorithm, <<"">>).
% @doc
% Sign <tt>Data</tt> with the given <tt>Algorithm</tt> and <tt>KeyOrPem</tt>.
%
% Supported algorithms :
% <ul>
% <li>hs256, hs384, hs512</li>
% <li>rs256, rs384, rs512</li>
% <li>es256, es384, es512</li>
% <li>none</li>
% </ul>
%
% This function support ext, nbt, iat, iss, sub, aud and jti.
%
% Example:
%
% <pre>
% Token = jwerl:sign(#{key =&gt; &lt;&lt;"Hello World"&gt;&gt;}, hs256, &lt;&lt;"s3cr3t k3y"&gt;&gt;).
% </pre>
% @end
-spec sign(Data :: map() | list(), Algorithm :: algorithm(), KeyOrPem :: binary()) -> binary().
sign(Data, Algorithm, KeyOrPem) when (is_map(Data) orelse is_list(Data)), is_atom(Algorithm), is_binary(KeyOrPem) ->
    encode(jsx:encode(Data), config_headers(#{alg => algorithm_to_binary(Algorithm)}), KeyOrPem).

% @equiv verify(Data, <<"">>, hs256, #{}, #{})
verify(Data) ->
    verify(Data, hs256, <<"">>, #{}, #{}).
% @equiv verify(Data, Algorithm, <<"">>, #{}, #{})
verify(Data, Algorithm) ->
    verify(Data, Algorithm, <<"">>, #{}, #{}).
% @equiv verify(Data, Algorithm, KeyOrPem, #{}, #{})
verify(Data, Algorithm, KeyOrPem) ->
  verify(Data, Algorithm, KeyOrPem, #{}, #{}).
% @equiv verify(Data, Algorithm, KeyOrPem, #{}, #{})
verify(Data, Algorithm, KeyOrPem, Claims) ->
  verify(Data, Algorithm, KeyOrPem, Claims, #{}).
% @doc
% Verify a JWToken according to the given <tt>Algorithm</tt>, <tt>KeyOrPem</tt> and <tt>Claims</tt>.
% This verifycation can ignore (<tt>CheckClaims =:= false</tt>) claims.
%
% This function support ext, nbt, iat, iss, sub, aud and jti.
%
% Options:
%
% <ul>
% <li><tt>exp_leeway</tt> : <tt>integer()</tt></li>
% <li><tt>iat_leeway</tt> : <tt>integer()</tt></li>
% </ul>
%
% Example :
%
% <pre>
% jwerl:verify(Token, hs256, &lt;&lt;"s3cr3t k3y"&gt;&gt;, #{sub =&gt; &lt;&lt;"hello"&gt;&gt;,
%                                                            aud =&gt; [&lt;&lt;"world"&gt;&gt;, &lt;&lt;"aliens"&gt;&gt;]}).
% </pre>
% @end
-spec verify(Data :: binary(), Algorithm :: algorithm(), KeyOrPem :: binary(), CheckClaims :: map() | list() | false, Opts :: map() | list()) ->
  {ok, map()} | {error, term()}.
verify(Data, Algorithm, KeyOrPem, Claims, Opts) ->
  case decode(Data, KeyOrPem, Algorithm) of
    {ok, TokenData} when is_map(Claims) orelse is_list(Claims) ->
      case check_claims(TokenData, Claims, Opts) of
        ok ->
          {ok, TokenData};
        Error ->
          Error
      end;
    Result ->
      Result
  end.

% @doc
% Return the header for a given <tt>JWToken</tt>.
%
% Example:
%
% <pre>
% jwerl:header(Token).
% </pre>
% @end
-spec header(Data :: binary()) -> map().
header(Data) ->
  decode_header(Data).

check_claims(TokenData, Claims, Opts) when is_map(Opts) ->
  check_claims(TokenData, Claims, maps:to_list(Opts));
check_claims(TokenData, Claims, Opts) when is_list(Opts) ->
  Now = os:system_time(seconds),
  claims_errors(
    [
     check_claim(TokenData, exp, false, fun(ExpireTime) ->
                                            ExpLeeway = proplists:get_value(exp_leeway, Opts, 0),
                                            Now < ExpireTime + ExpLeeway
                                        end, exp),
     check_claim(TokenData, iat, false, fun(IssuedAt) ->
                                            IatLeeway = proplists:get_value(iat_leeway, Opts, 0),
                                            IssuedAt - IatLeeway =< Now
                                        end, iat),
     check_claim(TokenData, nbf, false, fun(NotBefore) ->
                                            NotBefore =< Now
                                        end, nbf)
     | [
        check_claim(
          TokenData,
          Claim,
          true,
          fun(Value) ->
              claim_match(Expected, Value)
          end, Claim)
        || {Claim, Expected} <- get_claims(Claims)
       ]
    ], []);
check_claims(TokenData, Claims, _Opts) ->
  check_claims(TokenData, Claims, []).

claims_errors([], []) -> ok;
claims_errors([], List) -> {error, List};
claims_errors([ok|Rest], Acc) -> claims_errors(Rest, Acc);
claims_errors([{error, Error}|Rest], Acc) -> claims_errors(Rest, [Error|Acc]).

claim_match(Expected, Value) ->
  case is_string_or_uri(Value) of
    true ->
      case Expected of
        Value ->
          true;
        List when is_list(List) ->
          lists:member(Value, List);
        _Other ->
          false
      end;
    false ->
      false
  end.

check_claim(TokenData, Key, Required, F, FailReason) ->
  case get_claim(Key, TokenData) of
    error when Required =:= false ->
      %% Ignore if missing. If it has been correctly signed,
      %% this was intended.
      ok;
    error ->
      {error, FailReason};
    {ok, Value} ->
      %% Call back if found for custom checking logic
      case F(Value) of
        true -> ok;
        false -> {error, FailReason}
      end
  end.

get_claim(Claim, Map) when is_map(Map) ->
  maps:find(Claim, Map);
get_claim(Claim, List) when is_list(List) ->
  case lists:keyfind(Claim, 1, List) of
    {Claim, Value} -> {ok, Value};
    false -> error
  end.

get_claims(Map) when is_map(Map) ->
  maps:to_list(Map);
get_claims(List) when is_list(List) ->
  List.

encode(Data, #{alg := <<"none">>} = Options, _) ->
  encode_input(Data, Options);
encode(Data, Options, Key) ->
  Input = encode_input(Data, Options),
  <<Input/binary, ".", (signature(maps:get(alg, Options), Key, Input))/binary>>.

decode(Data, KeyOrPem, Algorithm) ->
  Header = decode_header(Data),
  case algorithm_to_atom(maps:get(alg, Header)) of
    Algorithm -> payload(Data, Algorithm, KeyOrPem);
    Algorithm1 -> {error, {invalid_algorithm, Algorithm1, Algorithm}}
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
  jsx:decode(base64_decode(Header), [return_maps, {labels, attempt_atom}]).

payload(Data, none, _) ->
  [_, Data1|_] = binary:split(Data, <<".">>, [global]),
  {ok, jsx:decode(base64_decode(Data1), [return_maps, {labels, attempt_atom}])};
payload(Data, Algorithm, Key) ->
  [Header, Data1, Signature] = binary:split(Data, <<".">>, [global]),
  {AlgMod, ShaBits} = algorithm_to_infos(Algorithm),
  case erlang:apply(AlgMod, verify, [ShaBits,
                                     Key,
                                     <<Header/binary, ".", Data1/binary>>,
                                     base64_decode(Signature)]) of
    true ->
      {ok, jsx:decode(base64_decode(Data1), [return_maps, {labels, attempt_atom}])};
    _ ->
      {error, invalid_signature}
  end.

encode_input(Data, Options) ->
  <<(base64_encode(jsx:encode(Options)))/binary, ".", (base64_encode(Data))/binary>>.

signature(Algorithm, Key, Data) ->
  {AlgMod, ShaBits} = algorithm_to_infos(Algorithm),
  Signature = erlang:apply(AlgMod, sign, [ShaBits, Key, Data]),
  base64_encode(Signature).

algorithm_to_atom(<<"HS256">>) -> hs256;
algorithm_to_atom(<<"RS256">>) -> rs256;
algorithm_to_atom(<<"ES256">>) -> es256;
algorithm_to_atom(<<"HS384">>) -> hs384;
algorithm_to_atom(<<"RS384">>) -> rs384;
algorithm_to_atom(<<"ES384">>) -> es384;
algorithm_to_atom(<<"HS512">>) -> hs512;
algorithm_to_atom(<<"RS512">>) -> rs512;
algorithm_to_atom(<<"ES512">>) -> es512;
algorithm_to_atom(A) when is_atom(A) -> A;
algorithm_to_atom(_) -> none.

algorithm_to_binary(hs256) -> <<"HS256">>;
algorithm_to_binary(rs256) -> <<"RS256">>;
algorithm_to_binary(es256) -> <<"ES256">>;
algorithm_to_binary(hs384) -> <<"HS384">>;
algorithm_to_binary(rs384) -> <<"RS384">>;
algorithm_to_binary(es384) -> <<"ES384">>;
algorithm_to_binary(hs512) -> <<"HS512">>;
algorithm_to_binary(rs512) -> <<"RS512">>;
algorithm_to_binary(es512) -> <<"ES512">>;
algorithm_to_binary(A) when is_binary(A) -> A;
algorithm_to_binary(_) -> <<"none">>.

algorithm_to_infos(Algorithm) ->
  case algorithm_to_binary(Algorithm) of
    <<"HS", ShaBits/binary>> ->
      {jwerl_hs, binary_to_integer(ShaBits)};
    <<"RS", ShaBits/binary>> ->
      {jwerl_rs, binary_to_integer(ShaBits)};
    <<"ES", ShaBits/binary>> ->
      {jwerl_es, binary_to_integer(ShaBits)};
    _ ->
      exit(invalid_algorithme)
  end.

conveniece_keys() ->
    registered_claim_names(),
    header_parameters(),
    miscellaneous(),
    ok.

registered_claim_names() ->
    iss,
    sub,
    aud,
    exp,
    nbf,
    iat,
    jti.

header_parameters() ->
    typ,
    cty.

miscellaneous() ->
    alg.

is_string_or_uri(Value) when is_binary(Value) ->
  size(trim(Value, both)) > 0;
is_string_or_uri(_Value) ->
  false.

trim(Binary, left) ->
  trim_left(Binary);
trim(Binary, right) ->
  trim_right(Binary);
trim(Binary, both) ->
  trim_left(trim_right(Binary)).

trim_left(<<C, Rest/binary>>) when C =:= $\s orelse
                                   C =:= $\n orelse
                                   C =:= $\r orelse
                                   C =:= $\t ->
  trim_left(Rest);
trim_left(Binary) -> Binary.

trim_right(Binary) ->
  trim_right(Binary, size(Binary)-1).

trim_right(Binary, Size) ->
  case Binary of
    <<Rest:Size/binary, C>> when C =:= $\s
                                 orelse C =:= $\t
                                 orelse C =:= $\n
                                 orelse C =:= $\r ->
      trim_right(Rest, Size - 1);
    Other ->
      Other
  end.
