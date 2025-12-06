#import "def.typ";
#import "format/lib.typ": DefFormatter, FormattedDef, single-version-formatted-def;

#let refine-part(part) = {
  if type(part) == array {
    stack(
      dir: ttb,
      ..part.map(p => $
        thin
        #p
        thin
      $)
    )
  } else {
    $thin part thin$
  }
}

#let group(..args) = args.pos()

#let inferrule(
  premise,
  conclusion,
) = {
  let premise = refine-part(premise);
  let conclusion = refine-part(conclusion);
  $
    frac(premise, conclusion)  
  $
}

#let rule-name(rule-def) = {
  rule-def.group-id + "-" + rule-def.id;
}

#let rule-tag(rule-def) = {
  let name = rule-name(rule-def);
  [
    #set text(font: "linux libertine")
    (#name)
  ]
}

#let rule-ref(uuid, rule-def) = {
  let name = rule-name(rule-def);
  show link: set text(fill: luma(0%))
  link(label(uuid), [Rule #smallcaps(name)]);
}


#let rule-formatter(
  line-spacing: 4pt,
  with-name: false,
  rule-ref: rule-ref, // (uuid: String, rule-def: Def) -> Content
  rule-tag: rule-tag, // (rule-def: Def) -> Content
  sizes: auto, // (Content, cramped: Bool) -> String
  cramped: false
) = DefFormatter(
  (figure-id, rule-def) => {
    let uuid = def.def-uuid(figure-id, rule-def.group-id, rule-def.id);
    let raw = rule-def.content;
    if sizes != auto {
      raw = sizes(raw, cramped: cramped);
    }
    let content = if with-name {
        show math.frac: set stack(spacing: line-spacing);
        [
          $
            #raw
            quad
            #rule-tag(rule-def)
          $#label(uuid)
          
        ]
      } else {
        show math.frac: set stack(spacing: line-spacing);
        [
          $
            #raw
          $#label(uuid)
        ]
      };

    let ref = rule-ref(uuid, rule-def);
    let refs = (:);
    refs.insert(uuid, ref);
    
    single-version-formatted-def(content, refs)
  }
)