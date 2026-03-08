# `Lucid MathML Core`

Domain-specific language for [MathML Core](https://w3c.github.io/mathml-core/) embedded in Haskell
as an extension of [Lucid](https://hackage.haskell.org/package/lucid2), a library for HTML.


```haskell
exampleFromCoreSpec :: Html ()
exampleFromCoreSpec = math_ [display_ "block"] do
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
