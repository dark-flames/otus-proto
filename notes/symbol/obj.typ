#let idx(i) = $v_#i$

#let subst(tm, subst) = {
  $#tm [#subst]$
}

#let compose(subst1, subst2) = {
  $#subst1 circle.stroked.tiny #subst2$
}