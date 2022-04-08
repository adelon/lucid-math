# `lucid-math`

DSL for [MathML Core](https://mathml-refresh.github.io/mathml-core/) in Haskell
as an extension of the DSL [`lucid`](https://hackage.haskell.org/package/lucid) for HTML.


```haskell
exampleFromCoreSpec :: Html ()
exampleFromCoreSpec = math_ [displayBlock_] do
    mrow_ do
        munderover_ do
            mo_ "∑"
            mrow_ (mi_ "n" *> mo_ "=" *> mn_ "1")
            mrow_ (mo_ "+" *> mn_ "∞")
        mfrac_ do
            mn_ "1"
            msup_ (mi_ "i" *> mn_ "2")
    mo_ "="
    mfrac_ do
        msup_ (mi_ "π" *> mn_ "2")
        mn_ "6"
```
