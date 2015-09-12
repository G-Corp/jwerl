-module(jwerl_es).
-include_lib("public_key/include/public_key.hrl").

-export([sign/3, verify/4]).

sign(ShaBits, Key, Data) ->
  ECPrivateKeyPem1 = case public_key:pem_decode(Key) of
                       [_, ECPrivateKeyPem] -> ECPrivateKeyPem;
                       [ECPrivateKeyPem] -> ECPrivateKeyPem
                     end,
  ECPrivateKey = public_key:pem_entry_decode(ECPrivateKeyPem1),
  public_key:sign(Data, algo(ShaBits), ECPrivateKey).

verify(ShaBits, Key, Data, Signature) ->
  [SPKI] = public_key:pem_decode(Key),
  #'SubjectPublicKeyInfo'{algorithm = Der} = SPKI,
  RealSPKI = public_key:der_decode('SubjectPublicKeyInfo', Der),
  #'SubjectPublicKeyInfo'{
     subjectPublicKey = Octets,
     algorithm = #'AlgorithmIdentifier'{ parameters = Params}
    } = RealSPKI,
  ECPoint = #'ECPoint'{point = Octets},
  EcpkParametersPem = {'EcpkParameters', Params, not_encrypted},
  ECParams = public_key:pem_entry_decode(EcpkParametersPem),
  ECPublicKey = {ECPoint, ECParams},
  public_key:verify(Data, algo(ShaBits), Signature, ECPublicKey).

algo(256) -> sha256;
algo(384) -> sha384;
algo(512) -> sha512.
