-module(jwerl_tests).

-include_lib("eunit/include/eunit.hrl").

jwerl_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_test(t_jwerl_default()),
    ?_test(t_jwerl_none()),
    ?_test(t_jwerl_sha()),
    ?_test(t_jwerl_rsa()),
    ?_test(t_jwerl_ecdsa()),
    ?_test(t_jwerl_no_claims()),
    ?_test(t_jwerl_claims()),
    ?_test(t_jwerl_header()),
    ?_test(t_jwerl_registered_claim_name()),
    ?_test(t_jwerl_not_registered_claim_name()),
    ?_test(t_jwerl_leeway())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_jwerl_leeway() ->
    Now = os:system_time(seconds),
    IssuedAtWithClockSkew1 = Now + 500,
    Data1 = #{key => <<"value">>, exp => Now + 1000, nbf => Now, iat => IssuedAtWithClockSkew1},
    ?assertMatch({error, [iat]}, jwerl:verify(
                                jwerl:sign(Data1, none),
                                none,
                                <<"">>,
                                #{},
                                #{iat_leeway => 250})),
    IssuedAtWithClockSkew2 = Now + 50,
    Data2 = #{key => <<"value">>, exp => Now + 1000, nbf => Now, iat => IssuedAtWithClockSkew2},
    ?assertMatch({ok, Data2}, jwerl:verify(
                                jwerl:sign(Data2, none),
                                none, <<"">>, #{}, #{iat_leeway => 250})),
    Data3 = #{key => <<"value">>, exp => Now - 500, nbf => Now, iat => Now },
    ?assertMatch({error, [exp]}, jwerl:verify(
                                jwerl:sign(Data3, none),
                                none, <<"">>, #{}, #{exp_leeway => 250})),
    Data4 = #{key => <<"value">>, exp => Now - 200, nbf => Now, iat => Now },
    ?assertMatch({ok, Data4}, jwerl:verify(
                                jwerl:sign(Data4, none),
                                none, <<"">>, #{}, #{exp_leeway => 250})).

t_jwerl_default() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(jwerl:sign(Data))),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, hs256, <<"s3cr3t k3y">>),
                             hs256,
                             <<"s3cr3t k3y">>)),
  ?assertMatch({error, {invalid_algorithm, none, hs256}},
               jwerl:verify(
                 jwerl:sign(Data, none))),
  ?assertMatch({error, {invalid_algorithm, none, hs256}},
               jwerl:verify(
                 jwerl:sign(Data, none),
                 hs256)).

t_jwerl_none() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, none),
                             none)).

t_jwerl_sha() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, hs256, <<"s3cr3t k3y">>),
                             hs256,
                             <<"s3cr3t k3y">>)),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, hs384, <<"s3cr3t k3y">>),
                             hs384,
                             <<"s3cr3t k3y">>)),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, hs512, <<"s3cr3t k3y">>),
                             hs512,
                             <<"s3cr3t k3y">>)).

t_jwerl_rsa() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, rs256, rsa_private_key()),
                             rs256,
                             rsa_public_key())),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, rs384, rsa_private_key()),
                             rs384,
                             rsa_public_key())),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, rs512, rsa_private_key()),
                             rs512,
                             rsa_public_key())).

t_jwerl_ecdsa() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, es256, ec_private_key()),
                             es256,
                             ec_public_key())),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, es384, ec_private_key()),
                             es384,
                             ec_public_key())),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, es512, ec_private_key()),
                             es512,
                             ec_public_key())),

  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, es512, ec_private_key(), #{raw => true}),
                             es512,
                             ec_public_key(), [], #{raw => true})).

t_jwerl_no_claims() ->
  Now = os:system_time(seconds),
  %% All three claim timestamps are invalid but check_claims => false
  Data1 = #{key => <<"value">>, exp => Now, nbf => Now + 10, iat => Now + 10},
  ?assertMatch({ok, Data1}, jwerl:verify(
                              jwerl:sign(Data1, none),
                              none, <<"">>, false)),
  %% No claims, ignore
  Data2 = #{key => <<"value">>},
  ?assertMatch({ok, Data2}, jwerl:verify(
                              jwerl:sign(Data2, none),
                              none, <<"">>, true)).

t_jwerl_claims() ->
  Now = os:system_time(seconds),
  %% All ok
  Data1 = #{key => <<"value">>, exp => Now + 10, nbf => Now, iat => Now},
  ?assertMatch({ok, Data1}, jwerl:verify(
                              jwerl:sign(Data1, none),
                              none, <<"">>, true)),
  %% Claim check fail conditions
  Data2 = #{key => <<"value">>, exp => Now, nbf => Now, iat => Now},
  ?assertMatch({error, [exp]}, jwerl:verify(
                                   jwerl:sign(Data2, none),
                                   none)),

  Data3 = #{key => <<"value">>, exp => Now + 10, nbf => Now, iat => Now + 10},
  ?assertMatch({error, [iat]}, jwerl:verify(
                                            jwerl:sign(Data3, none),
                                            none)),

  Data4 = #{key => <<"value">>, exp => Now + 10, nbf => Now + 10, iat => Now},
  ?assertMatch({error, [nbf]}, jwerl:verify(
                                         jwerl:sign(Data4, none),
                                         none)),

  Data5 = #{key => <<"value">>, sub => <<"hello">>},
  ?assertMatch({error, [sub]}, jwerl:verify(
                                 jwerl:sign(Data5, none),
                                 none, <<"">>,
                                 #{sub => <<"monde">>})),
  ?assertMatch({ok, Data5}, jwerl:verify(
                                 jwerl:sign(Data5, none),
                                 none, <<"">>,
                                 #{sub => <<"hello">>})),

  Data6 = #{key => <<"value">>, iss => <<"hello">>},
  ?assertMatch({error, [iss]}, jwerl:verify(
                                 jwerl:sign(Data6, none),
                                 none, <<"">>,
                                 #{iss => <<"monde">>})),
  ?assertMatch({ok, Data6}, jwerl:verify(
                                 jwerl:sign(Data6, none),
                                 none, <<"">>,
                                 #{iss => <<"hello">>})),

  Data7 = #{key => <<"value">>, jti => <<"hello">>},
  ?assertMatch({error, [jti]}, jwerl:verify(
                                 jwerl:sign(Data7, none),
                                 none, <<"">>,
                                 #{jti => <<"monde">>})),
  ?assertMatch({ok, Data7}, jwerl:verify(
                                 jwerl:sign(Data7, none),
                                 none, <<"">>,
                                 #{jti => <<"hello">>})),

  Data8 = #{key => <<"value">>, aud => <<"hello">>},
  ?assertMatch({error, [aud]}, jwerl:verify(
                                 jwerl:sign(Data8, none),
                                 none, <<"">>,
                                 #{aud => [<<"monde">>, <<"mundo">>]})),
  ?assertMatch({ok, Data8}, jwerl:verify(
                                 jwerl:sign(Data8, none),
                                 none, <<"">>,
                                 #{aud => [<<"hello">>, <<"monde">>, <<"mundo">>]})),
  ?assertMatch({ok, Data8}, jwerl:verify(
                                 jwerl:sign(Data8, none),
                                 none, <<"">>,
                                 #{aud => <<"hello">>})).

t_jwerl_header() ->
  Data = #{key => <<"value">>},
  DefaultHeader = #{typ => <<"JWT">>, alg => <<"HS256">>},
  ?assertMatch(DefaultHeader, jwerl:header(jwerl:sign(Data))).

t_jwerl_registered_claim_name() ->
    In = #{<<"sub">> => <<"s">>},
    Out = #{sub => <<"s">>},
    ?assertMatch({ok, Out}, jwerl:verify(jwerl:sign(In))).

t_jwerl_not_registered_claim_name() ->
    Data = #{<<"planet">> => <<"zorg">>},
    ?assertMatch({ok, Data}, jwerl:verify(jwerl:sign(Data))).

rsa_private_key() ->
  % openssl genrsa -out private_key.pem 4096
  <<"-----BEGIN RSA PRIVATE KEY-----\n",
    "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\n",
    "-----END RSA PRIVATE KEY-----">>.

rsa_public_key() ->
  % openssl rsa -pubout -in private_key.pem -out public_key.pem
  <<"-----BEGIN PUBLIC KEY-----\n",
    "MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA0HSV8/KmhlU/Z0QA1BQP\n",
    "Hbr19f8aIuE92PLAA8IDv+8yvr7oGFPX7WYW/WlaOguUkFNSleuFz1fphBWdoe+W\n",
    "xbm2/kRyahRAggnWq1QWERAJHIMfQ93R4hy0nfN4mepid13ZH6GEEGG7hFLQ2/Cr\n",
    "I0GvvfjcJGkP3FfcOzrE85YGMctFQEKnqleTpdk+6FuPiCu1RVKJPhEpJ+E7KOC1\n",
    "+yFnZozXyAxjyovxxMKMmQKs8FJ9nzgbmeNjn3wwNzZc1lzf4bSUxrPNolTHPzcL\n",
    "XHIgIVGMOuatrqLTxNVauMlvWwp3gzPuMGOS0mgG+vF9n+kPjvwis446rocgnIvv\n",
    "qdvRxPwNMN9tMbaNZt5kBF2DEMMF8bHl7vHGhkK12O7+ia8miclWNACrz5bSuX/z\n",
    "qhdYC60zkMs54h8TUMX78c7gNTCBcD54BjstCKZg/V9LCNb6VrPJRYkAzAacs4Es\n",
    "Jqw5iZ8YpzuYg4z9UxmJx1qusYeXye3iP6ZZZYhaQVftLIM5CdGTF0FSqIxbf8lK\n",
    "MJvRxIkwMyhdo5AFl50t3Pj+rSR/fsSrYAc+Zp8ZAeXGRjjdj86J649nSHRJtNpi\n",
    "kmrzbn4CRfjRruTJY4QduN5W2gPMHelZB+j7FscI1mylpcS4Ig5/SYutEP6qrwB6\n",
    "TYwKDoi8i9jnNBVR11kDfqcCAwEAAQ==\n",
    "-----END PUBLIC KEY-----">>.

ec_private_key() ->
  % openssl ecparam -out ec_key_priv.pem -name prime192v1 -genkey
  <<"-----BEGIN EC PARAMETERS-----\n",
    "BggqhkjOPQMBAQ==\n",
    "-----END EC PARAMETERS-----\n",
    "-----BEGIN EC PRIVATE KEY-----\n",
    "MF8CAQEEGK29VMTAIY/TktYdhI9y21ByURhl030sGaAKBggqhkjOPQMBAaE0AzIA\n",
    "BLJvq+6ubwBbk95EgUtmxXdoRy+ZTvkYMwFkML64pCEHiZUThM8URt9f2Ay/Hocx\n",
    "JQ==\n",
    "-----END EC PRIVATE KEY-----">>.

ec_public_key() ->
  % openssl ec -pubout -in ec_key_priv.pem -out ec_key_pub.pem
  <<"-----BEGIN PUBLIC KEY-----\n",
    "MEkwEwYHKoZIzj0CAQYIKoZIzj0DAQEDMgAEsm+r7q5vAFuT3kSBS2bFd2hHL5lO\n",
    "+RgzAWQwvrikIQeJlROEzxRG31/YDL8ehzEl\n",
    "-----END PUBLIC KEY-----">>.
