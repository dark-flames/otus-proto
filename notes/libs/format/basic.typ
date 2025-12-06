#import "types.typ": *;
#import "../helper.typ";

#let basic-def-formatter = DefFormatter(
  (figure-id, def) => {
    let def-label = def.def-label(figure-id, rule-def.group-id, rule-def.id);
    let content = [
      #label(def-label)
      #def.content
    ];
    FormattedDef(content, (ref(def-label, "Definition")))
  }
)

#let basic-formatter(
  default-def-formatter: basic-def-formatter, // DefFormatter
  def-formatters: (), // [(Def -> Bool, DefFormatter)]
) = Formatter(
  (defs, id, ..args) => {
    let entries = ();
    let def-contents = defs.map(def => {
      let f = def-formatters.find((searcher, formatter) => searcher(def));
      let formatter = if f != none { f.at(1) } else { default-def-formatter };
      let result = formatter(id, def);
      entries = helper.union(entries, result.at(1));
      result.at(0)
    });
    let uuid = figure-uuid(id);
    let content = [
      #figure(
        def-contents.join(),
        ..args
      )#label(uuid)
    ];

    entries.insert(uuid, link(label(uuid), [Figure. #rule-name(rule-def)]))

    DefFigure(
      id,
      content,
      entries
    )
  }
)