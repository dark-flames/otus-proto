#let Formatter(
  format, // ([Def], id: String, ..Args) -> DefFigure
) =  format

#let DefFormatter(
  format // ([Def], id: String, ..Args) -> (Content, Map<String, Content>)
) = format


#let DefFigure(id, content, refs) = (
  id: id,
  content: content,
  refs: refs
)

#let FormattedDef(contents, refs) = (
  contents: contents,
  refs: refs
)

#let single-version-formatted-def(content, refs) = FormattedDef((content, ), refs)

#let figure-uuid(figure-id) = {
  figure-id
}

#let ref-figure(def-figure) = {
  def-figure.refs.at(def-figure.id)
}

#let figure-label(def-figure) = {
  label(figure-uuid(def-figure.id))
}