

# JWErl #

Copyright (c) 2015, 2016, 2017, 2018, 2019 G-Corp - 2020 Gregoire Lejeune

__Version:__ 1.1.0

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).

[![All Contributors](https://img.shields.io/badge/all_contributors-5-orange.svg)](#contributors)
[![Hex.pm version](https://img.shields.io/hexpm/v/jwerl.svg?style=flat-square)](https://hex.pm/packages/jwerl)
[![Hex.pm downloads](https://img.shields.io/hexpm/dt/jwerl.svg?style=flat-square)](https://hex.pm/packages/jwerl)
[![License](https://img.shields.io/hexpm/l/jwerl.svg?style=flat-square)](https://hex.pm/packages/jwerl)
<br />
[![JWT](https://jwt.io/img/badge.svg)](https://jwt.io/)

__Warning, versions prior to 1.0.0 are affected by [this vulnerabilitie](https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/). Get up to date!__


## Support ##
* HS256, HS384, HS512
* RS256, RS384, RS512
* ES256, ES384, ES512



## Usage ##


### Erlang ###

```erlang

% signed by default (hs256)
Jwt = jwerl:sign([{name, <<"bob">>}]).
jwerl:verify(Jwt).

% signed by specifing method
Jwt = jwerl:sign([{name, <<"bob">>}], hs512).
jwerl:verify(Jwt, hs512).

% signed with secret key
Jwt = jwerl:sign([{name, <<"bob">>}], hs256, <<"kkey">>).
jwerl:verify(Jwt, hs256, <<"kkey">>).

% signed by none
Jwt = jwerl:sign([{name, <<"bob">>}], none).
jwerl:verify(Jwt, none).

% signed by RS512
{ok, PrivtPem} = file:read_file("path/to/rsa_private_key.pem"),
{ok, PublcPem} = file:read_file("path/to/rsa_public.pem"),
Jwt = jwerl:sign([{name, <<"bob">>}], rs512, PrivtPem).
jwerl:verify(Jwt, rs512, PublcPem).

% signed by ES256
{ok, PrivtPem} = file:read_file("path/to/es_private_key.pem"),
{ok, PublcPem} = file:read_file("path/to/es_public.pem"),
Jwt = jwerl:sign([{name, <<"bob">>}], es256, PrivtPem).
jwerl:verify(Jwt, es256, PublcPem).
```


### Elixir ###

```erlang

% signed by default (hs256)
jwt = Jwerl.sign([name: "bob"])
Jwerl.verify(jwt)

% signed by specifing method
jwt = Jwerl.sign([name: "bob"], :hs512)
Jwerl.verify(jwt, :hs512)

% signed with secret key
jwt = Jwerl.sign([name: "bob"], :hs256, "kkey")
Jwerl.verify(jwt, :hs256, "kkey")

% signed by none
jwt = Jwerl.sign([name: "bob"], none)
Jwerl.verify(jwt, :none)

% signed by RS512
{ok, private_pem} = File.read("path/to/rsa_private_key.pem")
{ok, public_pem} = File.read("path/to/rsa_public.pem")
jwt = Jwerl.sign([name: "bob"], :rs512, private_pem)
Jwerl.verify(jwt, :rs512, public_pem)

% signed by ES256
{ok, private_pem} = File.read("path/to/es_private_key.pem")
{ok, public_pem} = File.read("path/to/es_public.pem")
jwt = Jwerl.sign([name: "bob"], :es256, private_pem)
Jwerl.verify(jwt, :es256, public_pem)
```


## Licence ##

Copyright (c) 2015, 2016, 2017, 2018, 2019, G-Corp<br />
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
1. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
1. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://gitlab.com/glejeune/jwerl/tree/master/doc/jwerl.md" class="module">jwerl</a></td></tr></table>

## Contributors ##

Thanks goes to these wonderful people ([emoji key](https://github.com/kentcdodds/all-contributors#emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="http://spaceboyz.net/~astro/"><img src="https://avatars0.githubusercontent.com/u/12923?v=4" width="100px;" alt=""/><br /><sub><b>Astro</b></sub></a><br /><a href="https://github.com/G-Corp/jwerl/commits?author=astro" title="Code">💻</a> <a href="https://github.com/G-Corp/jwerl/commits?author=astro" title="Tests">⚠️</a></td>
    <td align="center"><a href="http://lejeun.es"><img src="https://avatars1.githubusercontent.com/u/15168?v=4" width="100px;" alt=""/><br /><sub><b>Gregoire Lejeune</b></sub></a><br /><a href="https://github.com/G-Corp/jwerl/commits?author=glejeune" title="Code">💻</a> <a href="#design-glejeune" title="Design">🎨</a> <a href="https://github.com/G-Corp/jwerl/commits?author=glejeune" title="Documentation">📖</a> <a href="https://github.com/G-Corp/jwerl/commits?author=glejeune" title="Tests">⚠️</a></td>
    <td align="center"><a href="https://github.com/michalwski"><img src="https://avatars3.githubusercontent.com/u/705828?v=4" width="100px;" alt=""/><br /><sub><b>Michał Piotrowski</b></sub></a><br /><a href="https://github.com/G-Corp/jwerl/commits?author=michalwski" title="Code">💻</a></td>
    <td align="center"><a href="https://hjianbo.me"><img src="https://avatars1.githubusercontent.com/u/13825269?v=4" width="100px;" alt=""/><br /><sub><b>JianBo He</b></sub></a><br /><a href="https://github.com/G-Corp/jwerl/commits?author=HJianBo" title="Code">💻</a> <a href="https://github.com/G-Corp/jwerl/commits?author=HJianBo" title="Documentation">📖</a> <a href="https://github.com/G-Corp/jwerl/commits?author=HJianBo" title="Tests">⚠️</a></td>
    <td align="center"><a href="https://maqbool.net"><img src="https://avatars1.githubusercontent.com/u/7526000?v=4" width="100px;" alt=""/><br /><sub><b>Mohammad Maqbool Alam</b></sub></a><br /><a href="https://github.com/G-Corp/jwerl/commits?author=m13m" title="Documentation">📖</a></td>
    <td align="center"><a href="https://github.com/ricardoccpaiva"><img src="https://avatars1.githubusercontent.com/u/2286067?v=4" width="100px;" alt=""/><br /><sub><b>Ricardo Paiva</b></sub></a><br /><a href="https://github.com/G-Corp/jwerl/commits?author=ricardoccpaiva" title="Code">💻</a> <a href="https://github.com/G-Corp/jwerl/commits?author=ricardoccpaiva" title="Tests">⚠️</a></td>
  </tr>
</table>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/kentcdodds/all-contributors) specification. Contributions of any kind welcome!
