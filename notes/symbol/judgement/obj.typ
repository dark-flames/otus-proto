#let ctx(ctx) = {
  $⊢ctx thick bold("ctx")_0$
}

#let ty(mctx, ctx, ty) = {
  $mctx|ctx ⊢ty thick bold("type")_0$
}


#let tm(mctx, ctx, t, A) = {
  let c = [#mctx|#ctx]
  $#c ⊢ #t : #A$
}

#let ctm(mctx, ctx, problem, t, A) = {
  let bar = $attach(|, br: problem)$
  let c = [#mctx#bar#ctx]
  $#c ⊢ #t : #A$
}
