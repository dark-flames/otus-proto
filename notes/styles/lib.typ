#let abstract(body) = {
  [
    #text(weight: "bold")[Abstract] #body
  ]
}

#let todo(content) = {
  [
    #text(fill: red)[TODO: #content]
  ]
}