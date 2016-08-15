% @hidden
-module(jwerl_hs).

-export([sign/3, verify/4]).

sign(ShaBits, Key, Data) ->
  crypto:hmac(algo(ShaBits), Key, Data).

verify(ShaBits, Key, Data, Signature) ->
  Signature == sign(ShaBits, Key, Data).


algo(256) -> sha256;
algo(384) -> sha384;
algo(512) -> sha512.
