
#let GCtx(ext: ()) = {
  let ext-segs = if ext.len() > 0 {
    let segs = ext.join($, $);
    $, #segs$
  } else {
    []
  };
  $Γ #ext-segs$
}


#let GVdash(ext: ()) = {
  $GCtx(ext: ext) ⊢ $
}

#let isCtx(Ctx) = {
  $⊢ Ctx$
}

#let EmptyCtx() = {
  $★$
}


