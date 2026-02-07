
#let ctx(ctx) = {
  $⊢ #ctx thick bold("ctx")_1$
}


#let ty(ty) = {
  $⊢ #ty thick bold("type")_1$
}

#let tm(ctx, t, A) = {
  $#ctx ⊢ #t : #A$
}