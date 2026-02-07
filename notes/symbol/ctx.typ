
#let ctxExt(ctx, ..ext) = {
  let ext = ext.pos();
  let ext-segs = if ext.len() > 0 {
    let segs = ext.join($, $);
    $, #segs$
  } else {
    []
  };
  $#ctx#ext-segs$
}

#let GCtx(..ext) = {
  ctxExt($Gamma$, ..ext)
}
#let TCtx(..ext) = {
  ctxExt($Theta$, ..ext)
}


#let GVdash(..ext) = {
  $GCtx(..ext) ⊢ $
}

#let EmptyCtx() = {
  $★$
}


