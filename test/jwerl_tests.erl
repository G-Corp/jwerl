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
    ?_test(t_jwerl_claims())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_jwerl_default() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(jwerl:sign(Data))),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{key => <<"s3cr3t k3y">>}),
                             #{key => <<"s3cr3t k3y">>})).

t_jwerl_none() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => none}),
                             #{alg => none})).

t_jwerl_sha() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"HS256">>, key => <<"s3cr3t k3y">>}),
                             #{alg => <<"HS256">>, key => <<"s3cr3t k3y">>})),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"HS384">>, key => <<"s3cr3t k3y">>}),
                             #{alg => <<"HS384">>, key => <<"s3cr3t k3y">>})),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"HS512">>, key => <<"s3cr3t k3y">>}),
                             #{alg => <<"HS512">>, key => <<"s3cr3t k3y">>})).

t_jwerl_rsa() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"RS256">>, key => rsa_private_key()}),
                             #{alg => <<"RS256">>, key => rsa_public_key()})),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"RS384">>, key => rsa_private_key()}),
                             #{alg => <<"RS384">>, key => rsa_public_key()})),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"RS512">>, key => rsa_private_key()}),
                             #{alg => <<"RS512">>, key => rsa_public_key()})).

t_jwerl_ecdsa() ->
  Data = #{key => <<"value">>},
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"ES256">>, key => ec_private_key()}),
                             #{alg => <<"ES256">>, key => ec_public_key()})),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"ES384">>, key => ec_private_key()}),
                             #{alg => <<"ES384">>, key => ec_public_key()})),
  ?assertMatch({ok, Data}, jwerl:verify(
                             jwerl:sign(Data, #{alg => <<"ES512">>, key => ec_private_key()}),
                             #{alg => <<"ES512">>, key => ec_public_key()})).

t_jwerl_no_claims() ->
  Now = os:system_time(seconds),
  %% All three claim timestamps are invalid but check_claims => false
  Data1 = #{key => <<"value">>, exp => Now, nbf => Now + 10, iat => Now + 10},
  ?assertMatch({ok, Data1}, jwerl:verify(
                              jwerl:sign(Data1, #{alg => none}),
                              #{alg => none, check_claims => false})),
  %% No claims, ignore
  Data2 = #{key => <<"value">>},
  ?assertMatch({ok, Data2}, jwerl:verify(
                              jwerl:sign(Data2, #{alg => none}),
                              #{alg => none, check_claims => true})).

t_jwerl_claims() ->
  Now = os:system_time(seconds),
  %% All ok
  Data1 = #{key => <<"value">>, exp => Now + 10, nbf => Now, iat => Now},
  ?assertMatch({ok, Data1}, jwerl:verify(
                              jwerl:sign(Data1, #{alg => none}),
                              #{alg => none, check_claims => true})),
  %% Claim check fail conditions
  Data2 = #{key => <<"value">>, exp => Now, nbf => Now, iat => Now},
  ?assertMatch({error, expired}, jwerl:verify(
                                   jwerl:sign(Data2, #{alg => none}),
                                   #{alg => none})),
  Data3 = #{key => <<"value">>, exp => Now + 10, nbf => Now, iat => Now + 10},
  ?assertMatch({error, future_issued_at}, jwerl:verify(
                                   jwerl:sign(Data3, #{alg => none}),
                                   #{alg => none})),
  Data4 = #{key => <<"value">>, exp => Now + 10, nbf => Now + 10, iat => Now},
  ?assertMatch({error, not_yet_valid}, jwerl:verify(
                                   jwerl:sign(Data4, #{alg => none}),
                                   #{alg => none})).

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

