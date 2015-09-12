-module(jwerl_rs).

-export([sign/3, verify/4]).

sign(ShaBits, Key, Data) ->
  [Entry] = public_key:pem_decode(Key),
  PeKey = public_key:pem_entry_decode(Entry),
  public_key:sign(Data, algo(ShaBits), PeKey).

verify(ShaBits, Key, Data, Signature) ->
  [Entry] = public_key:pem_decode(Key),
  PeKey = public_key:pem_entry_decode(Entry),
  public_key:verify(Data, algo(ShaBits), Signature, PeKey).

algo(256) -> sha256;
algo(384) -> sha384;
algo(512) -> sha512.
