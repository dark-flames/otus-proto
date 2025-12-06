#import "../helper.typ";
#let space(level) = (
  type: "space",
  level: level
)

#let newline = space(100)

#let infer-rule(premise, conclusion) = (
  type: "infer-rule",
  premise: if type(premise) == array { premise } else { (premise, ) },
  conclusion: if type(conclusion) == array { conclusion } else { (conclusion, ) },
)

#let is-infer-rule(rule) = {
  type(rule) == dictionary and rule.type == "infer-rule"
}

#let is-space(rule) = {
  type(rule) == dictionary and rule.type == "space"
}

#let break-points(rule) = {
  if is-infer-rule(rule) {
    let premise-breaks = rule.premise.map(break-points).fold((100, ), helper.union-set);
    let conclusion-breaks = rule.conclusion.map(break-points).fold((100, ), helper.union-set)
    let breaks = helper.union-set(premise-breaks, conclusion-breaks);
    breaks
  } else if is-space(rule) and rule.level > 0 {
    
    (rule.level,)
  } else {
    (100,)
  }
}