#import "../../libs/lib.typ": *;
#import "../../symbol/ctx.typ": *;
#import "../../symbol/judgment.typ": *;

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
