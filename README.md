# latex-live-snippets

Install with `stack install`, and then in latex via:

```latex
% set this to where your code is
\newcommand{\srcdir}{.}

\newcommand{\snip}[2]{\immediate\write18{latex-live-snippets \srcdir/#1.hs #2}\input{.latex-live-snippets/#1.#2.tex}}
```

Now, given a file `Test.hs`:

```haskell
zoo :: Int
zoo = 5

test :: Bool -> Bool
test True = id $ True
test _    = True  -- ! 1
```

we can call

```latex
\snip{Test}{test}
```

which will result in:

```latex
\begin{code}
test :: Bool -> Bool
test True = id \$ True
test \_    = True \ann{1}
\end{code}
```

It will also find type families, data definitions. Custom snippet areas can be
defined via comments  of the form `-- # name`.

