#import "@preview/shiroa:0.1.0": is-pdf-target, is-web-target, get-page-width
#import "typst-apollo/lib.typ": pages
#import pages: *

#import "styles/lib.typ": *


#show: project.with(
  title: "Core Theory of Otus",
  show-title: true,
  authors: (
    (
      name: "darkflames",
      email: "darkf1ames@pm.me",
    ),
  ),
  show-authors: true
)

#set text(size: 14pt)

#set par(justify: true)
#set heading(numbering: (..nums) => nums
    .pos()
    .map(str)
    .join("."))

#show math.equation: set text(size: 12pt)

= Overview
#include "content/overview.typ"

= Design
#include "content/design.typ"

/*


== Judgments

== Well-Formed Context

#import "libs/lib.typ": *;

#import "defs/mltt/lib.typ" as mltt;

#let def-formatter = flex-formatter(
  default-def-formatter: flex-rule-formatter(
    with-name: true,
    style: RuleStyle(space-size: 10pt)
  )
);

#let ctx-figure = def-formatter(mltt.ctx.group, "ctx");

#ctx-figure.content
*/



#bibliography("bibliography.bib")


