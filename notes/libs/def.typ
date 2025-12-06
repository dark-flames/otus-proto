#import "helper.typ"


#let Def(id, content, group-id: none) = (
  id: id,
  group-id: group-id,
  content: content
)

#let DefGroup(group-id, defs: ()) = (
  defs.map(def => {
    if type(def) == array {
      Def(def.first(), def.last(), group-id: group-id)
    } else {
      def.group-id = group-id
      def
    }
  })
)

#let def-group-module(group-id, mod) = (
  DefGroup(group-id, defs: dictionary(mod).pairs())
)

#let only(group, def-ids: ()) = {
  if def-ids.len() > 0 {
    group.filter(def => def-ids.contains(def.id))
  } else {
    group
  }
}

#let except(group, def-ids: ()) = {
  if def-ids.len() > 0 {
    group.filter(def => def-ids.contains(def.id) == false)
  } else {
    group
  }
}

#let groups(..groups) = {
  let defs = ();
  for group in groups.pos() {
    defs = defs + group
  }
  defs
}


#let def-uuid(figure-id, group-id, def-id) = {
  figure-id + "-" + group-id + "-" + def-id
}

#let ref-def(def-figure, def) = {
  def-figure.refs.at(def-uuid(def-figure.id, def.group-id, def.id))
}

#let def-label(def-figure, group-id, def-id) = {
  label(def-uuid(def-figure.id, group-id, def-id))
}




