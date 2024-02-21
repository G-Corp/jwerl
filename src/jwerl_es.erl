% @hidden
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
  [FirstByte | _] = binary_to_list(Signature),

  SignatureToVerify =
    case FirstByte of
      48 ->
        %% DER Encoded Signature. When encoded in DER, raw signature from r & s format
        %% becomes the following sequence of bytes:
        %% 0x30 b1 0x02 b2 (vr) 0x02 b3 (vs)
        Signature;
      _ ->
        SignatureLen = byte_size(Signature),
        {RBin, SBin} = split_binary(Signature, (SignatureLen div 2)),
        R = crypto:bytes_to_integer(RBin),
        S = crypto:bytes_to_integer(SBin),
        public_key:der_encode('ECDSA-Sig-Value', #'ECDSA-Sig-Value'{ r = R, s = S })
    end,

  public_key:verify(Data, algo(ShaBits), SignatureToVerify, ECPublicKey).

algo(256) -> sha256;
algo(384) -> sha384;
algo(512) -> sha512.
