#import "symbol.typ": *;
#import "../format/lib.typ": DefFormatter, FormattedDef;
#import "../def.typ";

#let rule-name(rule-def) = {
  rule-def.group-id + "-" + rule-def.id;
}

#let rule-tag(rule-def) = {
  let name = rule-name(rule-def);
  [
    #set text(font: "Libertinus Serif")
    (#smallcaps(name))
  ]
}

#let rule-ref(uuid, rule-def) = {
  let name = rule-name(rule-def);
  show link: set text(fill: luma(0%))
  link(label(uuid), [Rule #smallcaps(name)]);
}

#let RuleStyle(
  space-size: 2pt,
  line-padding: (x: 4pt, y: 4pt),
  scale: 80%,
  rule-ref: rule-ref, // (uuid: String, rule-def: Def) -> Content
  rule-tag: rule-tag, // (rule-def: Def) -> Content
  sizes: auto, // (Content, cramped: Bool) -> String
  cramped: false
) = (
  space-size: space-size,
  line-padding: line-padding,
  scale: scale,
  rule-ref: rule-ref,
  rule-tag: rule-tag,
  sizes: auto, // (Content, cramped: Bool) -> String
  cramped: false
)

#let split-part(as-math, depth, part, level, style) = context{
  let rows = ();
  let row = ();
  let row-grid(row) = {
    grid(
      columns: row.len(),
      column-gutter: style.space-size,
      align: center + bottom,
      ..row
    )
  }
  for item in part {
    if is-space(item)  {
      if item.level >= level {
        rows.push(row-grid(row));
        row = ();
      }
    } else {
      row.push(as-math(
        item,
        depth: depth + 1,
        level: level,
        style: style
      ));
    }
  }

  if row.len() > 0 {
    rows.push(row-grid(row));
  }
  grid(
    columns: 1,
    rows: auto,
    inset: style.line-padding,
    align: center,
    ..rows
  )
}

#let as-math(rule, depth: 0, level: 100, style: RuleStyle()) = context {
  if type(rule) == content {
    rule
  } else if is-infer-rule(rule) {
    let premise = split-part(as-math, depth, rule.premise, level, style);
    let conclusion = split-part(as-math, depth, rule.conclusion, level, style);
    let content = $
      frac(premise, conclusion)
    $
    if depth != 0 {
      content = scale(x: style.scale, y: style.scale, origin: center + bottom, content)
    }
    content
  }
}

#let flex-rule-formatter(
  with-name: false,
  style: RuleStyle(),
) = DefFormatter(
  (figure-id, rule-def) => {
    let uuid = def.def-uuid(figure-id, rule-def.group-id, rule-def.id);
    let breakpoints = break-points(rule-def.content).rev();
    let contents = ();
    for l in breakpoints {
      let raw = as-math(rule-def.content, level: l, style: style);
      if style.sizes != auto {
        raw = sizes(raw, cramped: cramped);
      }
      let tag = (style.rule-tag)(rule-def);
      let content = if with-name {
        [
          $
            #raw
            quad
            #tag
          $#label(uuid)
        ]
      } else {
        [
          $
            #raw
          $#label(uuid)
        ]
      };

      contents.push(content);
    }
    

    let ref = (style.rule-ref)(uuid, rule-def);
    let refs = (:);
    refs.insert(uuid, ref);
    
    FormattedDef(contents, refs)
  }
)