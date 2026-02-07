#import "../../symbol": *;

#let empty = Def(
  "Empty", 
  infer-rule(
    $$,
    $wfCtx(EmptyCtx())$
  )
)

#let ext = Def(
  "Ext", 
  infer-rule(
    ($wfCtx(GCtx())$, $wfTy(GCtx(), A)$),
    $wfCtx(#GCtx(ext: ($A$, )))$
  )
)


#let group = DefGroup("Ctx", defs: (empty, ext));
