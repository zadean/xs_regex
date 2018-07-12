xs_regex
=====

An OTP library that translates XML Schema regular expressions (described here https://www.w3.org/TR/xmlschema-2/#regexs) into the Erlang flavor.

One cool thing here is the ability to use range subtraction. So, [a-z-[s-u]] returns [a-rv-z].
Also, all language blocks are implemented and the Unicode 10.0 table is used. 

The XML Multi-Character Escape sequences can also be used. That is: \i, \I, \c, \C, \d, \D, \w, \W. 
These match (or negate) the initial name characters, name characters, decimal digits, and word characters (non punctuation, separator, and other).

Back-references and positional replacements are also implemented. Examples to come.

#### Usage

```erlang
1> xs_regex:translate("[a-z-[s-u]]").
{ok,"[a-rv-z]"}
```

Arabic decimal digits:

```erlang
2> xs_regex:translate("[\\p{IsArabic}-[\\P{Nd}]]").
{ok,"[\\x{660}-\\x{669}\\x{6F0}-\\x{6F9}]"}
```

Opening XML tag:

```erlang
3> xs_regex:translate("<\\i\\c*>").
{ok,"<(?-i:[\\x{3A}A-Z\\x{5F}a-z\\x{C0}-\\x{D6}\\x{D8}-\\x{F6}\\x{F8}-\\x{2FF}\\x{370}-\\x{37D}\\x{37F}-\\x{1FFF}\\x{200C}-\\x{200D}\\x{2070}-\\x{218F}\\x{2C00}-\\x{2FEF}\\x{3001}-\\x{D7FF}\\x{F900}-\\x{FDCF}\\x{FDF0}-\\x{FFFD}\\x{10000}-\\x{EFFFF}])(?-i:[\\x{2D}-\\x{2E}0-\\x{3A}A-Z\\x{5F}a-z\\x{B7}\\x{C0}-\\x{D6}\\x{D8}-\\x{F6}\\x{F8}-\\x{37D}\\x{37F}-\\x{1FFF}\\x{200C}-\\x{200D}\\x{203F}-\\x{2040}\\x{2070}-\\x{218F}\\x{2C00}-\\x{2FEF}\\x{3001}-\\x{D7FF}\\x{F900}-\\x{FDCF}\\x{FDF0}-\\x{FFFD}\\x{10000}-\\x{EFFFF}])*>"}
```

Upper-case Greek:

```erlang
4> xs_regex:translate("^(?:[\\p{IsGreek}-[\\P{Lu}]]+)$").
{ok,"^(?:[\\x{370}\\x{372}\\x{376}\\x{37F}-\\x{383}\\x{386}\\x{388}-\\x{38F}\\x{391}-\\x{3AB}\\x{3CF}\\x{3D2}-\\x{3D4}\\x{3D8}\\x{3DA}\\x{3DC}\\x{3DE}\\x{3E0}\\x{3E2}\\x{3E4}\\x{3E6}\\x{3E8}\\x{3EA}\\x{3EC}\\x{3EE}\\x{3F4}\\x{3F7}\\x{3F9}-\\x{3FA}\\x{3FD}-\\x{3FF}]+)$"}
```

Using non-ascii codepoints in a regex:

```erlang
5> {ok,R} = xs_regex:normalize("^what(?:[&#50806;-&#50809;])s\\sthat$").
{ok,[94,119,104,97,116,40,63,58,91,50806,45,50809,93,41,115,
     92,115,116,104,97,116,36]}
6> xs_regex:translate(R).
{ok,"^what(?:[\\x{C676}-\\x{C679}])s(?-i:[\\x{9}\\x{A}\\x{D}\\x{20}])that$"}

7> {ok,R1} = xs_regex:normalize("^what(?:[&#xABC0;-&#xABCF;])s\\sthis$").
{ok,[94,119,104,97,116,40,63,58,91,43968,45,43983,93,41,115,
     92,115,116,104,105,115,36]}
8> xs_regex:translate(R1).
{ok,"^what(?:[\\x{ABC0}-\\x{ABCF}])s(?-i:[\\x{9}\\x{A}\\x{D}\\x{20}])this$"}
```

Build
-----

    $ rebar3 compile
