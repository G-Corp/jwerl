

# JWErl #

Copyright (c) 2015, 2016 G-Corp

__Version:__ 0.1.0

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).

[![Build Status](https://travis-ci.org/emedia-project/jwerl.svg?branch=master)](https://travis-ci.org/emedia-project/jwerl)
[![Hex.pm version](https://img.shields.io/hexpm/v/jwerl.svg?style=flat-square)](https://hex.pm/packages/jwerl)
[![Hex.pm downloads](https://img.shields.io/hexpm/dt/jwerl.svg?style=flat-square)](https://hex.pm/packages/jwerl)
[![License](https://img.shields.io/hexpm/l/jwerl.svg?style=flat-square)](https://hex.pm/packages/jwerl)

## Support
- HS256, HS384, HS512
- RS256, RS384, RS512
- ES256, ES384, ES512

## Usage ##

```erlang
% signed by default (hs256)
Jwt = jwerl:sign([{name, <<"bob">>}]).
jwerl:verify(Jwt).

% signed by specifing method
Jwt = jwerl:sign([{name, <<"bob">>}], hs512).
jwerl:verify(Jwt).

% signed with secret key
Jwt = jwerl:sign([{name, <<"bob">>}], hs256, <<"kkey">>).
jwerl:verify(Jwt, <<"kkey">>).

% signed by none
Jwt = jwerl:sign([{name, <<"bob">>}], none).
jwerl:verify(Jwt).

% signed by RS512
{ok, PrivtPem} = file:read_file("path/to/rsa_private_key.pem"),
{ok, PublcPem} = file:read_file("path/to/rsa_public.pem"),
Jwt = jwerl:sign([{name, <<"bob">>}], rs512, PrivtPem).
jwerl:verify(Jwt, PublcPem).

% signed by ES256
{ok, PrivtPem} = file:read_file("path/to/es_private_key.pem"),
{ok, PublcPem} = file:read_file("path/to/es_public.pem"),
Jwt = jwerl:sign([{name, <<"bob">>}], es256, PrivtPem).
jwerl:verify(Jwt, PublcPem).
```

Of course, you can find more details in `test/jwerl_tests.erl`.

## Licence ##

Copyright (c) 2015, 2016, G-Corp<br />
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
1. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
1. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/emedia-project/jwerl/blob/master/doc/jwerl.md" class="module">jwerl</a></td></tr></table>

