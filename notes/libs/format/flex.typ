#import "types.typ": *;
#import "../helper.typ";
#import "basic.typ": basic-def-formatter;


#let adjust-layout(rows, content-widths, column-gutter, max-width, front-load) = {
  let n = content-widths.len();
  let layout = range(0, n + 1)
    .map(i => range(0, rows + 1).map(j => if i == n and j == 0 {
          (
          rows: (),
          max: 0,
          min: 114514,
        )
      } else {
        none
      })
    );

  for s in range(n - 1, -1, step: -1) {
    for e in range(s + 1, n + 1) {
        let gutters = if e == s + 1 { 0 } else { e - s - 2 };
        let row-width = content-widths.slice(s, e).sum() + gutters * column-gutter;
        if row-width > max-width and e != s + 1 {
          break;
        }
        let next = layout.at(e);
        for l in range(rows - 1, -1, step: -1) {
          let next = layout.at(e).at(l);
          if next == none {
            continue;
          } else {
            let current-diff = calc.max(row-width, next.max) - calc.min(row-width, next.min);
            let result = layout.at(s).at(l + 1);
            let result-diff = if result == none {
              114514
            } else {
              result.max - result.min;
            }
            
            if current-diff < result-diff or (front-load and current-diff == result-diff) {
              layout.at(s).at(l + 1) = (
                rows: (e - s, ..next.rows),
                max: calc.max(row-width, next.max),
                min: calc.min(row-width, next.min)
              );
            }
          }
        }
      }
  }

  layout.first().at(rows).rows
}

#let flex-formatter(
  front-load: true,
  column-gutter: 20pt,
  row-gutter: 10pt,
  align: center + bottom,
  default-def-formatter: basic-def-formatter, // DefFormatter
  def-formatters: (), // [(Def -> Bool, DefFormatter)]
) = Formatter(
  (
    defs,
    id,
    ..args
  ) => {
    defs = defs.rev();
    let entries = (:);
    let contents = ();
    let w = ();

    for def in defs {
      let f = def-formatters.find((searcher, formatter) => searcher(def));
      let formatter = if f != none { f.last() } else { default-def-formatter };
      let result = formatter(id, def);
      entries = helper.union(entries, result.refs);
      contents.push(result.contents);
    }
    let figure-uuid = figure-uuid(id);
    entries.insert(figure-uuid, ref(label(figure-uuid)));

    let content = layout(box => {
      let contents = contents.map(versions => {
        versions.find(c => measure(c).width.pt() <= box.width.pt())
      });
      let cur = 0;
      let row-count = 0;
      let content-widths = ();
      while cur < contents.len() {
        let row-width = 0;
        while cur < contents.len()  {
          let next = contents.at(cur);
          let next-width = measure(next).width.pt();

          let gutter-width = if row-width > 0 { column-gutter.pt() } else { 0 };
          let new-width = row-width + next-width + gutter-width;
          if new-width < box.width.pt() {
            content-widths.push(next-width);
            cur += 1;
            row-width = new-width;
          } else if row-width == 0 {
            content-widths.push(next-width);
            cur += 1;
            row-width = new-width;
          } else {
            break;
          }
        };
        row-count += 1;
      }

      let layout = adjust-layout(
        row-count,
        content-widths,
        column-gutter.pt(),
        box.width.pt(),
        front-load
      );
      let rows = ();

      for count in layout.rev() {
        let row = ();
        for i in range(0, count) {
          row.push(contents.pop());
        }
        rows.push(grid(
          columns: row.len(),
          align: align,
          column-gutter: column-gutter,
          ..row
        ));
      }
      
      [
        #figure(
          grid(
            rows: rows.len(),
            row-gutter: row-gutter,
            align: align,
            ..rows
          ),
          ..args,
        )#label(figure-uuid)
      ]
    });
    DefFigure(
      id,
      content,
      entries
    )
  }
)