#let union(l, r) = {
  for pair in r.pairs() {
    l.insert(pair.first(), pair.last());
  }
  l
}

#let union-set(l, r) = {
  for item in r {
    if l.contains(item) == false {
      l.push(item);
    }
  }
  l.sorted()
}

#let repeat(str, n) = {
  let res = "";
  for i in range(n) {
    res = res + str;
  }
  res
}

#let repeat-array(i, n) = {
  let res = ();
  for i in range(n) {
    res.push(i)
  }
  
  res
}