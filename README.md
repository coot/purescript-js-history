# PureScript js history

The module provides a unified interface for managing histories using:
 * browser history based on [`DOM.HTML.History`](https://pursuit.purescript.org/packages/purescript-dom/)
 * memory history implemented using [`Ref`](https://pursuit.purescript.org/packages/purescript-refs)

There common type for them is
```purescript
type History e state =
  { state :: Eff (history:: HISTORY, dom :: DOM | e) (Maybe state)
  , back :: Eff (history:: HISTORY, dom :: DOM | e ) Unit
  , forward :: Eff (history:: HISTORY, dom :: DOM | e) Unit
  , go :: Delta -> Eff (history:: HISTORY, dom :: DOM | e) Unit
  , pushState :: state -> DocumentTitle -> URL -> Eff (history :: HISTORY, dom :: DOM | e) Unit
  , replaceState :: state -> DocumentTitle -> URL -> Eff (history :: HISTORY, dom :: DOM | e) Unit 
  }
```

Note that if you want to use browser history `state` has to be an instance of
`IsForeign` and `AsForeign` classes.

The memory history also adds `addEventListener` function with a similar type to
the one you'd use when attaching events to the DOM.  For the browser history
just use the `DOM.Event.EventTarget` module to attach events on the window object.
