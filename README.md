# elm-sparklines

Line and columns sparklines charts with optional interactive brushing 
(selection and labelling), based on 
[elm-visualization](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest).

![line with brushing](examples/assets/line-with-brushing.gif)
![columns with brushing](examples/assets/columns-with-brushing.gif)
![line facets with brushing](examples/assets/line-facets-with-brushing.gif)

## Getting started

Install from your elm project:

```
$ elm install ericgj/elm-sparklines
```

For color and label size configuration, you will also need to install 
`elm-community/typed-svg`, and use `TypedSvg.Types.Paint` and `TypedSvg.Types.Length`.

For brushing, you will need to install `gampleman/elm-visualization`, and 
create and maintain a brush in your model as described in the
[documentation](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Brush).

The [Example/Brush](examples/src/Example/Brush.elm) and [Main](examples/src/Main.elm) 
example modules show how to do this.


