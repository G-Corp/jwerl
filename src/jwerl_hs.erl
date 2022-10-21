% @hidden
-module(jwerl_hs).

-export([sign/3, verify/4]).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 24).
-define(HMAC(S, K, D), crypto:mac(hmac, algo(S), K, D)).
-elif(?OTP_RELEASE >= 21).
-define(HMAC(S, K, D), crypto:hmac(algo(S), K, D)).
-endif.
-else.
-define(HMAC(S, K, D), crypto:hmac(algo(S), K, D)).
-endif.

sign(ShaBits, Key, Data) ->
  ?HMAC(ShaBits, Key, Data).

verify(ShaBits, Key, Data, Signature) ->
  Signature == sign(ShaBits, Key, Data).


algo(256) -> sha256;
algo(384) -> sha384;
algo(512) -> sha512.
