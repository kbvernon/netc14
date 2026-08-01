#let article(
  title: [],
  subtitle: none,
  authors: (),
  date: [],
  abstract: [],
  keywords: [],
  target_journal: [],
  correspondence: [],
  font: "libertinus serif",
  fontsize: 12pt,
  doc,
) = {
  set page(
    paper: "us-letter",
    margin: (x: 1in, y: 1in),
    footer: context {
      let page-num = counter(page).get().first()
      if page-num > 1 {
        align(center)[#(page-num - 1)]
      }
    }
  )
  set par(justify: false)
  set text(
    lang: "en",
    region: "US",
    font: font,
    size: fontsize
  )
  set heading(numbering: "1.1.")
  show math.equation: set text(weight: "regular")

  show heading: set block(above: 2 * fontsize, below: 1.2 * fontsize)
  show heading.where(level: 1): set block(above: 3.0 * fontsize)
  show heading.where(level: 2): set block(above: 2.2 * fontsize)

  show figure: set align(center)
  show figure: it => place(
    top,
    float: true,
    clearance: 2.5em,
    block(width: 100%, align(center, it))
  )

  show figure.caption: set align(left)
  show figure.caption: set text(size: 0.9em)
  show figure.caption: it => context {
    show: block.with(inset: (x: 5%))
    if it.numbering != none and it.supplement != none {
      let sequence = [].func()
      let supplement = it.supplement
      let numbers = if it.numbering != none {
        it.counter.display(it.numbering)
      }
      let is-empty-supplement = {
        it.supplement.func() == sequence and it.supplement.children.len() == 0
      }
      if not is-empty-supplement { supplement += [~] }
      strong[#supplement#numbers]
      it.separator
    }
    it.body
  }

  set list(indent: 0.25in)
  set enum(indent: 0.25in)

  show cite: set text(rgb("#467886"))
  show ref: set text(rgb("#467886"))

  // title page
  text(size: 1.5em)[#title]
  if subtitle != none { 
    parbreak()
    text(size: 1.25em)[#subtitle] 
  }
  v(1em)  

  // authors
  let unique-affiliations = authors.map(a => { a.affiliation }).flatten().dedup()
  let affiliation-map = (:)
  for (i, aff) in unique-affiliations.enumerate() {
    affiliation-map.insert(aff, i + 1)
  }
  authors.map(author => {
    let author-affs = if type(author.affiliation) == array { author.affiliation } else { (author.affiliation,) }
    let aff-nums = author-affs.map(aff => str(affiliation-map.at(aff)))
    [#author.name#super[#aff-nums.join(",")]]
  }).join(", ")
  v(1em)

  // affiliations
  for (i, aff) in unique-affiliations.enumerate() {
    [#super[#(i + 1)] #text(size: 0.85em, style: "italic")[#aff]]
    linebreak()
  }
  v(1em)

  // date
  [*Last updated:* #date]
  v(1em)

  // abstract
  [*Abstract* #linebreak() #abstract]
  v(1em)

  // keywords
  [*Keywords:* #keywords]
  v(1em)

  // target journal
  [*Target Journal:* #target_journal]
  v(1em)

  // correspondence
  [*Correspondence:* #correspondence]
  pagebreak()

  // main
  set par(leading: 1.1em, spacing: 2em)

  // tables opt out of the document's loose leading/spacing
  set table(
    inset: (x: 10pt, y: 10pt),
    stroke: (x, y) => (
      top: if y == 1 { 1pt + black } else if y > 1 { 0.3pt + luma(180) },
      bottom: 1pt + black
    )
  )
  show table: set par(leading: 0.5em, spacing: 0.5em)
  show table.cell.where(y: 0): set text(size: 0.85em, weight: 600)

  show raw.where(block: true): set block(
    fill: none,
    stroke: (left: 2pt + luma(200)),
    inset: (left: 1em, rest: 0.5em),
    radius: 0pt
  )

  doc
}
