

# Module jwerl #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-algorithm">algorithm()</a> ###


<pre><code>
algorithm() = hs256 | hs384 | hs512 | rs256 | rs384 | rs512 | es256 | es384 | es512 | none
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#header-1">header/1</a></td><td>
Return the header for a given <tt>JWToken</tt>.</td></tr><tr><td valign="top"><a href="#sign-1">sign/1</a></td><td>Equivalent to <a href="#sign-3"><tt>sign(Data, hs256, &lt;&lt;""&gt;&gt;)</tt></a>.</td></tr><tr><td valign="top"><a href="#sign-2">sign/2</a></td><td>Equivalent to <a href="#sign-3"><tt>sign(Data, Algorithm, &lt;&lt;""&gt;&gt;)</tt></a>.</td></tr><tr><td valign="top"><a href="#sign-3">sign/3</a></td><td>
Sign <tt>Data</tt> with the given <tt>Algorithm</tt> and <tt>KeyOrPem</tt>.</td></tr><tr><td valign="top"><a href="#verify-1">verify/1</a></td><td>Equivalent to <a href="#verify-5"><tt>verify(Data, &lt;&lt;""&gt;&gt;, hs256, #{}, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#verify-2">verify/2</a></td><td>Equivalent to <a href="#verify-5"><tt>verify(Data, Algorithm, &lt;&lt;""&gt;&gt;, #{}, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td>Equivalent to <a href="#verify-5"><tt>verify(Data, Algorithm, KeyOrPem, #{}, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#verify-4">verify/4</a></td><td>Equivalent to <a href="#verify-5"><tt>verify(Data, Algorithm, KeyOrPem, #{}, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#verify-5">verify/5</a></td><td>
Verify a JWToken according to the given <tt>Algorithm</tt>, <tt>KeyOrPem</tt> and <tt>Claims</tt>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="header-1"></a>

### header/1 ###

<pre><code>
header(Data::binary()) -&gt; map()
</code></pre>
<br />

Return the header for a given `JWToken`.

Example:

```

 jwerl:header(Token).
```

<a name="sign-1"></a>

### sign/1 ###

<pre><code>
sign(Data::map()) -&gt; binary()
</code></pre>
<br />

Equivalent to [`sign(Data, hs256, <<"">>)`](#sign-3).

<a name="sign-2"></a>

### sign/2 ###

<pre><code>
sign(Data::map(), Algorithm::<a href="#type-algorithm">algorithm()</a>) -&gt; binary()
</code></pre>
<br />

Equivalent to [`sign(Data, Algorithm, <<"">>)`](#sign-3).

<a name="sign-3"></a>

### sign/3 ###

<pre><code>
sign(Data::map() | list(), Algorithm::<a href="#type-algorithm">algorithm()</a>, KeyOrPem::binary()) -&gt; binary()
</code></pre>
<br />

Sign `Data` with the given `Algorithm` and `KeyOrPem`.

Supported algorithms :

* hs256, hs384, hs512

* rs256, rs384, rs512

* es256, es384, es512

* none


This function support ext, nbt, iat, iss, sub, aud and jti.

Example:

```

 Token = jwerl:sign(#{key => <<"Hello World">>}, hs256, <<"s3cr3t k3y">>).
```

<a name="verify-1"></a>

### verify/1 ###

`verify(Data) -> any()`

Equivalent to [`verify(Data, <<"">>, hs256, #{}, #{})`](#verify-5).

<a name="verify-2"></a>

### verify/2 ###

`verify(Data, Algorithm) -> any()`

Equivalent to [`verify(Data, Algorithm, <<"">>, #{}, #{})`](#verify-5).

<a name="verify-3"></a>

### verify/3 ###

`verify(Data, Algorithm, KeyOrPem) -> any()`

Equivalent to [`verify(Data, Algorithm, KeyOrPem, #{}, #{})`](#verify-5).

<a name="verify-4"></a>

### verify/4 ###

`verify(Data, Algorithm, KeyOrPem, Claims) -> any()`

Equivalent to [`verify(Data, Algorithm, KeyOrPem, #{}, #{})`](#verify-5).

<a name="verify-5"></a>

### verify/5 ###

<pre><code>
verify(Data::binary(), Algorithm::<a href="#type-algorithm">algorithm()</a>, KeyOrPem::binary(), CheckClaims::map() | list() | false, Opts::map()) -&gt; {ok, map()} | {error, term()}
</code></pre>
<br />

Verify a JWToken according to the given `Algorithm`, `KeyOrPem` and `Claims`.
This verifycation can ignore (`CheckClaims =:= false`) claims.

This function support ext, nbt, iat, iss, sub, aud and jti.

Example :

```

 jwerl:verify(Token, hs256, <<"s3cr3t k3y">>, #{sub => <<"hello">>,
                                                            aud => [<<"world">>, <<"aliens">>]}).
```

