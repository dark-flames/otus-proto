#import "../../libs/lib.typ": *;
#import "../../symbol/ctx.typ": *;
#import "../../symbol/judgment.typ": *;

#let empty = Def(
  "Empty", 
  infer-rule(
    $$,
    $isCtx(EmptyCtx())$
  )
)

#let ext = Def(
  "Ext", 
  infer-rule(
    ($isCtx(GCtx())$, $GVdash() isTy(A)$),
    $isCtx(#GCtx(ext: ($A$, )))$
  )
)


#let group = DefGroup("Ctx", defs: (empty, ext));
