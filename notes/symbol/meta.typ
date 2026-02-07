#import "helper.typ": *

#let inner(ctx, ty) = {
  $"Inner"parened(args(ctx, ty))$
}

#let consistent(ctx, problem, subst, tm) = {
  $"Ok"parened(args(ctx, problem, subst, tm))$
}

#let error() = {
  $"Error"$
}

#let cext(tm, problem, b, B) = {
  $"cext"parened(args(tm, problem, #b, #B))$
}