#let parened(c) = {
  $paren.l #c paren.r$
}

#let args(..l) = {
  let segs = l.pos().join($ comma$);
  $#segs$
}