// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}




/// NOTE OFCE — typst template
#import "@preview/icu-datetime:0.1.2": fmt-datetime, fmt-date

///// STYLE ELEMENTS

  // Colour definition
  #let grey0 =  rgb("#030303")
  #let grey1 =  rgb("#6B6B6B")
  #let grey2 =  rgb("#A6A6A6")
  #let grey3 =  rgb("#D6D6D6")
  #let scpored = rgb("#e6142d")
  #let scpodarkred = rgb("#770C19")

  // Font definition
  #let main_title_font = "Open Sans"
  #let serif_font = "Open Sans"


#let note(
  title: none,
  subtitle: none,
  authors: none,
  abstract: none,
  first_publish: none,
  number: none,
  language: "fr",
  leading: 0.6em,
  spacing: 1em,
  first-line-indent: 0cm,
  linkcolor: rgb(0, 0, 0),
  paper: "a4",
  region: "FR",
  font: ("Open Sans", "Arial"),
  fontsize: 10pt,
  section-numbering: none,
  bibliography-title: "Références",
  bibliography-style: "apa",
  cols: 1,
  col-gutter: 4.2%,
  doc,
) = {

  // Date formatting
  let main_date = if first_publish != none { first_publish.text } else { none }

  let pretty_date = if main_date != none {
    let date_decomp = main_date.split("-")
    let year_fp = int(date_decomp.at(0))
    let month_fp = int(date_decomp.at(1))
    let day_fp = int(date_decomp.at(2))
    let date_formatted = datetime(year: year_fp, month: month_fp, day: day_fp)
    fmt-date(date_formatted, length: "long", locale: language)
  }

  // Link / cite colours
  show link: set text(fill: linkcolor)
  show cite: set text(fill: linkcolor)
 show figure.where(kind: "quarto-float-apptbl"): set block(breakable: true)
 show figure.where(kind: table): set block(breakable: true)
 
  set bibliography(title: bibliography-title, style: bibliography-style)

  // Page settings — header alternates page numbers on subsequent pages
  set page(
    paper: paper,
    margin: (inside: 2.5cm, outside: 2.5cm, top: 2.5cm, bottom: 2.5cm),
    numbering: none,
    footer: none,
    header-ascent: 40%,
    header:
      context if here().page() == 1 {
        // No header on the title page
      } else {
        let pn = counter(page).display()
        if calc.even(here().page()) {
          align(left)[#text(pn, size: 9pt, fill: grey1)]
        } else {
          align(right)[#text(pn, size: 9pt, fill: grey1)]
        }
        v(-0.4em)
        line(start: (0cm, 0cm), end: (100%, 0cm),
          stroke: (thickness: 0.5pt, paint: grey1))
      },
  )

  // Paragraph and text settings
  set par(
    justify: true,
    leading: leading,
    first-line-indent: first-line-indent,
    spacing: spacing,
  )

  set text(
    region: region,
    font: font,
    size: fontsize,
    lang: language,
  )

  set heading(numbering: section-numbering)
  show heading.where(level: 1): it => block(width: 100%, below: 1em, above: 1.25em)[
    #set text(size: fontsize*1.3, weight: "bold", fill: scpored)
    #it
  ]
  show heading.where(level: 2): it => block(width: 100%, below: 1em, above: 1.25em)[
    #set text(size: fontsize*1.15, weight: "bold")
    #it
  ]
  show heading.where(level: 3): it => block(width: 100%, below: 0.8em, above: 1.2em)[
    #set text(size: fontsize*1.05, style: "italic")
    #it
  ]
  show heading.where(level: 4): it => box(
    inset: (top: 0em, bottom: 0em, left: 0em, right: 1em),
    text(size: 1em, weight: "bold", it),
  )
  show heading.where(level: 5): it => box(
    inset: (top: 0em, bottom: 0em, left: 0em, right: 1em),
    text(size: 1em, weight: "bold", style: "italic", it),
  )

  /* ---------------- Title page ---------------- */

  // Top row: logo top-left, date top-right
  grid(
    columns: (1fr, 1fr),
    column-gutter: 1em,
    align(left + top)[
      #image("/_extensions/ofce/note/logos/ofce_m.png", width: 3cm)
    ],
    align(right + top)[
      #if pretty_date != none {
        text(pretty_date, size: 10pt, fill: grey1)
      }
      #if number != none and number != [] {
        linebreak()
        text([Note nº #number], size: 10pt, fill: grey1, style: "italic")
      }
    ],
  )

  v(1.5cm)

  // Title and subtitle
  text(title, size: 22pt, weight: "bold", fill: scpored, font: main_title_font)
  if subtitle != none {
    v(0.5em)
    block(text(subtitle, size: 14pt, fill: grey1, font: main_title_font))
  }

  v(1.5em)

  // Authors — right justified
  if authors != none {
    align(right)[
      #grid(
        rows: authors.len(),
        row-gutter: 0.4em,
        ..authors.map(author => align(right)[
          #text(author.name, weight: "bold", size: 10pt)
          #if author.affiliation != none and author.affiliation != [] [
            , #text(author.affiliation, style: "italic", size: 10pt)
          ]
        ])
      )
    ]
  }

  v(1.5em)

  // Abstract
  if abstract != none and abstract != [] {
    block(
      fill: rgb("#F2F2F2"),
      inset: 1em,
      width: 100%,
      text(abstract, style: "italic", size: 9.5pt),
    )
  }

  v(1em)
  line(start: (0cm, 0cm), end: (100%, 0cm),
    stroke: (thickness: 0.5pt, paint: grey1))
  v(1em)

  // Body
  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: col-gutter, doc)
  }
}

// Remove gridlines from tables
#set table(
  inset: 6pt,
  stroke: none,
)
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "a4",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
  columns: 1,
)

#show: doc => note(
  title: [Ajustement budgétaire 2026-2030],
  number: [1],
  authors: (
          (
        name: [Xavier Timbeau],
        affiliation: [],
      ),
      
      ),
  first_publish: [2026-07-07],
  language: "fr",
  paper: "a4",
  font: ("Open Sans",),
  section-numbering: "1.1.1",
  doc,
)

= Principaux résultats
<principaux-résultats>
- Une cible de dette de 120% demande une une impulsion cumulée de 2,92~points de PIB (approx. 90~milliards d'euros de 2026) en 2032. Le solde public atteint 3,0% en 2034.

- La stabilisation de la dette en 2032 (ou cible de solde) implique une impulsion cumulée de 2,63% du PIB, avec un maximum de dette public plus élevé à 123,2% du PIB à terme (soit 3,2~point de PIB de dette en plus).

- Un taux souverain supérieur de 0,5~point accroît l'impulsion cumulée de 0,3~points de PIB en 2032.

Ces résultats découlent de simulations détaillées ci-dessous mis en oeuvre dans #cite(<psmt>, form: "prose") et #cite(<timbeau2025>, form: "prose").

== Conformité au RAA
<sec-main>
L'ajustement « optimal » -- tel que défini plus loin, voir principales hypothèses -- conduit à une impulsion de #strong[\-0,77~points de PIB] en 2027 et une impulsion cumulée de 2,92~points de PIB en 2032 (approx. 85~milliards d'euro au PIB de 2026). C'est un résultat qui découle d'un déficit initial important (5,1% en 2025) et d'un objectif de dette à brève échéance (2037). L'ajustement en 2026 est de -0,5% ce qui correspond au #link("https://www.tresor.economie.gouv.fr/Articles/b16f3103-ed47-4c37-9362-11ff01e12ae5/files/1d29351b-0187-4837-a3cd-e4ea7b0e4ce1")[Rapport d'Avancement Annuel 2026 publié le 22 avril 2026]. Les conséquences de cet ajustement sont le creusement de l'écart de production en 2027 et en 2028. A partir de 2029, les impulsions sont plus faibles et la dynamique du multiplicateur fait que l'ajustement de 2027 perd en effet sur l'écart de production. Cependant, parce qu'il y a chaque année une impulsion négative, même moindre, l'écart de production reste négatif jusu'en 2035 (#ref(<fig-optimal>, supplement: [graphique])).

#figure([
#{set text(font: ("Open Sans",) , size: 6pt); table(
  columns: 12,
  align: (left,right,right,right,right,right,right,right,right,right,right,right,),
  table.header(table.cell(align: bottom + left, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2025], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2026], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2027], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2028], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2029], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2030], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2031], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2032], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2033], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); LT (2050)], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); Max. 25-50],),
  table.hline(),
  table.cell(align: horizon + left)[Croissance du PIB#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[  0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,06~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,20~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,19~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,18~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,01~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Ecart de production#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,07~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,02~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,67~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,51~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,35~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −1,16~],
  table.cell(align: horizon + left)[Croissance du PIB potentiel#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Inflation#text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]]], table.cell(align: horizon + right)[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,82~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,74~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,58~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,55~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,53~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,58~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,72~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Taux de chômage#text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]]], table.cell(align: horizon + right)[  7,70~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,41~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,10~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,90~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,71~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  7,52~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  6,98~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  8,43~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Solde public#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,67~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,95~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,64~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,39~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,19~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,06~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,20~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −2,96~],
  table.cell(align: horizon + left)[Solde public primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,75~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,88~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,46~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,57~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,22~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,06~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,25~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,58~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,59~],
  table.cell(align: horizon + left)[Solde public structurel primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,85~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,91~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,17~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,10~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,30~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,42~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,57~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,57~],
  table.cell(align: horizon + left)[Impulsion budgétaire primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −1,06~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,77~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,46~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,39~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,27~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,20~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,01~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont PO#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,23~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,17~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,14~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,10~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,06~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont dépense#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,23~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,17~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,14~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,10~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,06~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Cumul de l\'impulsion primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,27~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,73~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,12~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,46~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,73~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,92~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −3,04~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −3,20~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −3,20~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Taux souverain courant#text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,35~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,27~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,27~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,29~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[---],
  table.cell(align: horizon + left)[Taux souverain pondéré#text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]]], table.cell(align: horizon + right)[  2,02~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,61~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,68~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,75~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  2,81~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,24~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Dette publique#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[115,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[117,51~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[119,51~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[120,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,20~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,54~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,67~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,63~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[121,41~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[120,10~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[121,67~],
  table.cell(align: horizon + left)[Charge d\'intérêts#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  2,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,63~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,82~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,96~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,07~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,17~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,25~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,32~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,78~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,80~],
  table.hline(),
  table.footer(table.cell(colspan: 12)[#block[
    #text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]] évolution en % annuel, volume #text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]] % du PIB #text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]] Minimum #text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]] déflateur du PIB, évolution en % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]] % de la population active #text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]] 10~ans, % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]] % annuel
    ]],
    table.cell(colspan: 12)[Données AMECO printemps 2026, première année de simulation : 2026],
    table.cell(colspan: 12)[Simulation debtwatch v2.0 (github.com/OFCE/dwr), France, stochastique (256 tirages), valeurs moyennes],
    table.cell(colspan: 12)[],),
)}
], caption: figure.caption(
position: top, 
[
Ajustement optimal, RAA en 2026
]), 
kind: "quarto-float-tbl", 
supplement: "Tableau", 
)
<tbl-optimal1>


Cet ajustement limite la hausse de la dette publique à 121,6% du PIB (#ref(<tbl-optimal1>, supplement: [tableau])), bien que l'intervalle de confiance soit assez important (#ref(<fig-optimal>, supplement: [graphique])), en particulier sur la dette publique.

Notons que cet écart est construit sous des hypothèses particulières @timbeau2025 et doit être interprété en fonction. En simplifiant à l'extrême, il s'agit de la prise en compte de la variabilité historique de la croissance, mais en supposant que la moyenne (de la croissance) est stable et qu'il y a peu d'autocorrélation dans ce processus (il est stationaire). Cette hypothèse induit un retour à la moyenne assez sûr de l'écart de production et donc limite le renforcement d'une déviation médiane de la trajectoire de dette.

Dans ce scénario, la dette publique passe par un maximum en 2031 et le solde public français est inférieur à -3,0% en 2033. A long terme, il est en dessous de -3% parce que pour ce niveau de dette le solde stabilisant la dette est de l'ordre de -3,3% ($- 2 \, 75$). Ceci conduit à un profil en bosse pour le solde public : il faut atteindre un solde plus haut pour réduire la dette, puis revenir au déficit stabilisant la dette.

== Coût de l'inaction : pas d'impulsion en 2027
<coût-de-linaction-pas-dimpulsion-en-2027>
Le #ref(<tbl-optimal2>, supplement: [tableau]) décrit un monde légèrement différent. Au lieu d'une impulsion de -0,77% du PIB en 2027, on suppose une impulsion nulle. L'optimisation reporte l'effort après 2027, ce qui est un résultat central dans le calcul d'une politique budgétaire optimale : le #emph[backloading] n'est justifié qu'en cas d'une forte préférence pour le présent, une préférence pour des impulsions les moins variées possibles ou des multiplicateurs budgétaires qui varient dans le cycle.

Le maximum de dette est plus important d'n point, 122,7% en 2031 contre 121,7% (en 2031). Le fait qu'avec un ajustement moins précoce l'écart de taux avec des obligations de référence puisse se réduire n'est pas pris en compte et accentuerait le contraste avec le scénario principal. Il existe cependant peu d'éléments empiriques pour justifier une variation franche de cet écart, et la nature #emph[forward-looking] des déterminants de l'écart de taux incite à la prudence : pas grand chose ne sépare ces deux trajectoires et donc elles ne sont pas plus rassurantes ou inquiétantes l'une que l'autre, tout au plus la seconde produisant un ajustement moins précoce, elle pourrait être associée à un taux souverain plus tendu.

#figure([
#{set text(font: ("Open Sans",) , size: 6pt); table(
  columns: 12,
  align: (left,right,right,right,right,right,right,right,right,right,right,right,),
  table.header(table.cell(align: bottom + left, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2025], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2026], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2027], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2028], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2029], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2030], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2031], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2032], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2033], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); LT (2050)], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); Max. 25-50],),
  table.hline(),
  table.cell(align: horizon + left)[Croissance du PIB#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[  0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,87~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,27~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,27~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,26~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,01~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Ecart de production#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,42~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,30~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,91~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,66~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,42~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −1,30~],
  table.cell(align: horizon + left)[Croissance du PIB potentiel#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Inflation#text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]]], table.cell(align: horizon + right)[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,82~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,78~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,64~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,58~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,50~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,55~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,72~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Taux de chômage#text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]]], table.cell(align: horizon + right)[  7,70~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,20~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,40~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,36~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,90~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  7,64~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  6,98~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  8,40~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Solde public#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,67~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,95~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,72~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,11~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −2,93~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,20~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −2,84~],
  table.cell(align: horizon + left)[Solde public primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,75~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,33~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,17~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,41~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,58~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,59~],
  table.cell(align: horizon + left)[Solde public structurel primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,85~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,07~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,49~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,08~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,48~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,61~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,58~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,63~],
  table.cell(align: horizon + left)[Impulsion budgétaire primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −1,06~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,07~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,57~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,41~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,32~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,23~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont PO#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,54~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,11~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,06~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont dépense#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,54~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,11~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,06~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Cumul de l\'impulsion primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,57~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,14~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,56~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,88~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −3,11~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −3,23~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −3,20~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −3,26~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Taux souverain courant#text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,35~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,27~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,27~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,27~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,29~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[---],
  table.cell(align: horizon + left)[Taux souverain pondéré#text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]]], table.cell(align: horizon + right)[  2,02~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,61~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,68~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,75~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  2,80~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,24~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Dette publique#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[115,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[117,51~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[119,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,05~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[122,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[122,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[122,63~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[122,42~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[121,99~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[120,10~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[122,63~],
  table.cell(align: horizon + left)[Charge d\'intérêts#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  2,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,83~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,98~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,10~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,20~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,27~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,34~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,78~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,80~],
  table.hline(),
  table.footer(table.cell(colspan: 12)[#block[
    #text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]] évolution en % annuel, volume #text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]] % du PIB #text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]] Minimum #text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]] déflateur du PIB, évolution en % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]] % de la population active #text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]] 10~ans, % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]] % annuel
    ]],
    table.cell(colspan: 12)[Données AMECO printemps 2026, première année de simulation : 2026],
    table.cell(colspan: 12)[Simulation debtwatch v2.0 (github.com/OFCE/dwr), France, stochastique (256 tirages), valeurs moyennes],
    table.cell(colspan: 12)[],),
)}
], caption: figure.caption(
position: top, 
[
Ajustement optimal, inaction en 2027
]), 
kind: "quarto-float-tbl", 
supplement: "Tableau", 
)
<tbl-optimal2>


#figure([
#box(image("note-ajustement-2026-XT_files/figure-typst/fig-optimal-1.png", width: 100.0%))
], caption: figure.caption(
position: top, 
[
Ajustement optimal
]), 
kind: "quarto-float-fig", 
supplement: "Graphique", 
)
<fig-optimal>


= Annexes
<annexes>
#heading(level: 2, numbering: none)[Annexe 1 : Principales hypothèses]
<annexe-1-principales-hypothèses>
+ On a supposé une croissance potentielle de 1% par an à terme. A moyen terme, la croissance potentielle est un peu supérieure du fait d'une baisse du chômage structurel vers 7% de la population active.

+ L'inflation est ancrée à 1,75% à long terme. En 2026, en suposant que le prix du pétrole est revenu à sa valeur d'avant la Guerre du Golfe III, l'inflation en 2026 en moyenne annuelle s'établirait juste au dessus de 1,8% par an.

+ Le solde public et la dette publique sont calés sur la dernière notification de la dette publique française (mars 2026). Les autres données macroéconomique pour l'année 2025 sont celle de la base AMECO de mai 2026. Elles diffèrent des comptes nationaux de mai 2026 à la marge.

+ L'écart de production est calé sur l'analyse de la DGTrésor. AMECO donne un écart de production de -0.9% du PIB potentiel. Cela ajouterait de l'ordre de 0,2% de croissance à court terme dans les scénarios de projection.

+ La prime sur le taux souverain par rapport à la croissance potentielle nominale s'accroît encore en 2027 puis décroît lentement.

+ L'optimisation est une optimisation sous contrainte d'atteindre la cible de dette (120%) à une date donnée (2037) et de s'y tenir ensuite. Les impulsions sont calculées en fonction en minimisant une fonction objectif qui minimise la perte quadratique d'écart de croissance et la variance des impulsions. Cette optmisation intègre les effets de retour des impulsions sur l'activité et donc les écarts de production.

+ Une optimisation alternative consistant à stabiliser la dette indépendamment de son niveau en 2032 est décrite en annexe 3.

#heading(level: 2, numbering: none)[Annexe 2 : Trajectoires au fil de l'eau]
<annexe-2-trajectoires-au-fil-de-leau>
La trajectoire au fil de l'eau est là à titre de référence. elle consiste à dérouler à partir des conditions initiales la logique de la dynamique du modèle sans impulsion budgétaire. Les propriétés du modèle sont très simple, avec d'un côté le retour à l'équilibre de l'écart de production et de l'autre le calcul de l'évolution de la dette publique par l'accumulation des déficits structurels et de la charge d'intérêt. L'inflation dépend, autour de la trajectoire, de l'écart de production (que ce soit pas un argument de type Philips ou un ajustement par le compte extérieur). La condition initiale en 2025 est celle d'un écart de production peu ouvert (conforme à l'hypothèse DGTrésor). L'appréciation de la position dans le cycle de la Commission Européenne est un peu plus négative, ce qui justifierait un peu plus de croissance dans le court terme.

La trajectoire au fil de l'eau est construite sur une impulsion budgétaire nulle, c'est-à-dire que le taux de prélèvement obligatoire est constant et que la dépense publique est constante en point de PIB potentiel. Cette trajectoire est déjà un ajustement budgétaire si on tient compte d'évolutions tendacielles ou votées comme la dépense sociale, la loi de programmation pluri-annuelle militaire ou les impôts exceptionnels qui doivent s'éteindre en 2026 ou en 2027. Le fil de l'eau choisi n'a donc rien de spontané mais illustre plutôt les dynamiques à l'œuvre.

De ce point de vue, la stabilité du déficit structurel ajouté à la hausse de la charge d'intérêt conduit à un accroissement du déficit public. A l'horizon de 2031, la hausse ainsi induite du déficit public est de l'ordre d'un point de PIB, ce résultat dépendant des hypothèses retenues sur les taux d'intérêt souverain courant et pondéré. Notons que la modélisation du taux pondéré, avec un modèle auto-régressif, minore potentiellement la hausse du taux pondéré à l'horizon de la décennie. A plus long terme, cette déviation s'accentue, conduisant la dette publique à 200% du PIB en 2050. Ceci constitue indéniablement une dynamique explosive mais relativement lente, en 25 années la dette s'accroît de 85~points de PIB, soit 3,4~points par an.

#figure([
#{set text(font: ("Open Sans",) , size: 6pt); table(
  columns: 12,
  align: (left,right,right,right,right,right,right,right,right,right,right,right,),
  table.header(table.cell(align: bottom + left, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2025], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2026], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2027], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2028], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2029], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2030], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2031], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2032], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2033], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); LT (2050)], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); Max. 25-50],),
  table.hline(),
  table.cell(align: horizon + left)[Croissance du PIB#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[  0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,35~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,22~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,12~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,06~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Ecart de production#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,42~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,22~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,05~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,07~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,08~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,08~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,09~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −0,42~],
  table.cell(align: horizon + left)[Croissance du PIB potentiel#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Inflation#text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]]], table.cell(align: horizon + right)[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,82~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,78~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,70~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,70~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,68~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,66~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,69~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,73~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Taux de chômage#text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]]], table.cell(align: horizon + right)[  7,70~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,65~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,20~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,10~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,05~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  7,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  6,91~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  8,04~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Solde public#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,67~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,95~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,10~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,33~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,47~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,60~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −7,59~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,67~],
  table.cell(align: horizon + left)[Solde public primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,75~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,33~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,23~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,10~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,09~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −2,09~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −2,08~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −2,08~],
  table.cell(align: horizon + left)[Solde public structurel primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,85~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,12~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,12~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,12~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −2,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −2,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −2,12~],
  table.cell(align: horizon + left)[Impulsion budgétaire primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −1,06~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont PO#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont dépense#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Cumul de l\'impulsion primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −0,50~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Taux souverain courant#text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,35~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,30~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,30~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,30~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,30~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,30~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[---],
  table.cell(align: horizon + left)[Taux souverain pondéré#text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]]], table.cell(align: horizon + right)[  2,02~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,61~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,69~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,76~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  2,82~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,24~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Dette publique#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[115,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[117,51~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[119,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[120,59~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[122,27~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[124,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[126,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[128,29~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[130,50~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[177,72~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[199,56~],
  table.cell(align: horizon + left)[Charge d\'intérêts#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  2,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,81~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,96~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,10~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,38~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,52~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  5,51~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  5,75~],
  table.hline(),
  table.footer(table.cell(colspan: 12)[#block[
    #text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]] évolution en % annuel, volume #text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]] % du PIB #text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]] Minimum #text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]] déflateur du PIB, évolution en % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]] % de la population active #text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]] 10~ans, % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]] % annuel
    ]],
    table.cell(colspan: 12)[Données AMECO printemps 2026, première année de simulation : 2026],
    table.cell(colspan: 12)[Simulation debtwatch v2.0 (github.com/OFCE/dwr), France, stochastique (256 tirages), valeurs moyennes],
    table.cell(colspan: 12)[],),
)}
], caption: figure.caption(
position: top, 
[
Ajustement optimal, -0,5% d'impulsion en 2026
]), 
kind: "quarto-float-tbl", 
supplement: "Tableau", 
)
<tbl-fildle>


#figure([
#box(image("note-ajustement-2026-XT_files/figure-typst/fig-fildle-1.png", width: 100.0%))
], caption: figure.caption(
position: top, 
[
Trajectoire au fil de l'eau
]), 
kind: "quarto-float-fig", 
supplement: "Graphique", 
)
<fig-fildle>


== Annexe 3 : cible de déficit à la place d'une cible de dette
<annexe-3-cible-de-déficit-à-la-place-dune-cible-de-dette>
Nous procédons ici à un calcul sensiblement différent, au moins dans le principe. Il s'agit de stabiliser la dette public à une date donnée indépendamment de son niveau à une date donnée (2032 en l'occurrence). Le schéma d'optmisation est assez simple, puisqu'il faut atteindre le solde stabilisant la dette (qui dans un monde stationnaire et avec un écart $r - g$ nul est 0) en partant d'un solde dégradé. En première approximation une réduction régulière (quasi linéaire) du point de départ jusqu'au point d'arrivée en 2032 est la trajectoire optimale. Quelques rétroactions et surtout la dynamique du multiplicateur budgétaire modifient marginalement cette première approximation.

Le #ref(<tbl-sp>, supplement: [tableau]) résulte de l'optimisation complète. La dette est stabilisée à une valeur proche de 123,2% du PIB, un ratio supérieur d'1,5~point à celui obtenu dans le scénario de cible de dette à 120% plus haut (#ref(<sec-main>, supplement: [section])). L'impulsion nécessaire est sensiblement inférieure (un cumul de 2,52~points de PIB en 2032 contre 2,92~points de PIB, soit une différence de 12~milliards d'euros au PIB de 2026).

#figure([
#{set text(font: ("Open Sans",) , size: 6pt); table(
  columns: 12,
  align: (left,right,right,right,right,right,right,right,right,right,right,right,),
  table.header(table.cell(align: bottom + left, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2025], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2026], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2027], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2028], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2029], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2030], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2031], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2032], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2033], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); LT (2050)], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); Max. 25-50],),
  table.hline(),
  table.cell(align: horizon + left)[Croissance du PIB#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[  0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,26~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,14~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,12~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,11~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Ecart de production#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,98~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,88~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,77~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,68~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,58~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,48~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,38~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,14~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −0,98~],
  table.cell(align: horizon + left)[Croissance du PIB potentiel#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Inflation#text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]]], table.cell(align: horizon + right)[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,82~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,75~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,63~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,61~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,58~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,55~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,59~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,72~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Taux de chômage#text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]]], table.cell(align: horizon + right)[  7,70~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,05~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,91~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,78~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,65~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  7,53~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  6,90~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  8,38~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Solde public#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,67~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,57~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,49~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,22~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,98~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,79~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,62~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,49~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,63~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,27~],
  table.cell(align: horizon + left)[Solde public primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,75~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,94~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,68~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,26~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,90~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,34~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,13~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,31~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,38~],
  table.cell(align: horizon + left)[Solde public structurel primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,85~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,48~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,26~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,89~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,58~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,32~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,11~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,05~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,24~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,36~],
  table.cell(align: horizon + left)[Impulsion budgétaire primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −1,06~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,66~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,36~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,31~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,26~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,21~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,16~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont PO#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,33~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,10~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,08~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,06~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont dépense#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,33~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,10~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,08~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,06~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Cumul de l\'impulsion primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,74~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,05~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,31~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,52~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −2,68~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −2,86~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −2,98~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Taux souverain courant#text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,35~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,28~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,29~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[---],
  table.cell(align: horizon + left)[Taux souverain pondéré#text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]]], table.cell(align: horizon + right)[  2,02~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,52~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,61~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,68~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,75~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  2,81~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,24~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Dette publique#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[115,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[117,51~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[119,47~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[120,46~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,41~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[122,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[122,72~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[123,13~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[123,36~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[125,18~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[127,56~],
  table.cell(align: horizon + left)[Charge d\'intérêts#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  2,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,63~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,82~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,96~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,08~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,28~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,36~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,94~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,97~],
  table.hline(),
  table.footer(table.cell(colspan: 12)[#block[
    #text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]] évolution en % annuel, volume #text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]] % du PIB #text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]] Minimum #text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]] déflateur du PIB, évolution en % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]] % de la population active #text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]] 10~ans, % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]] % annuel
    ]],
    table.cell(colspan: 12)[Données AMECO printemps 2026, première année de simulation : 2026],
    table.cell(colspan: 12)[Simulation debtwatch v2.0 (github.com/OFCE/dwr), France, stochastique (256 tirages), valeurs moyennes],
    table.cell(colspan: 12)[],),
)}
], caption: figure.caption(
position: top, 
[
Ajustement optimal avec cible de solde en 2032, RAA en 2026
]), 
kind: "quarto-float-tbl", 
supplement: "Tableau", 
)
<tbl-sp>


== Annexe 4 : Impact de taux d'intérêt plus élévés
<annexe-4-impact-de-taux-dintérêt-plus-élévés>
La variante présentée dans cette annexe repose sur des taux d'intérêt plus élevés à partir de 2028, accentuant l'écart observé aujourd'hui.

A l'horizon de l'année 2032, l'effort budgétaire serait supérieur d'un peu moins de 0,3~point de PIB (à peu près 8~milliards d'euros), ce qui constitue une bonne règle pour approximer l'effet des taux d'intérêt sur les trajectoires d'ajustement.

#figure([
#{set text(font: ("Open Sans",) , size: 6pt); table(
  columns: 12,
  align: (left,right,right,right,right,right,right,right,right,right,right,right,),
  table.header(table.cell(align: bottom + left, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); ], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2025], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2026], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2027], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2028], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2029], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2030], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2031], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2032], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); 2033], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); LT (2050)], table.cell(align: bottom + right, fill: rgb(255, 255, 255, 0%))[#set text(size: 1.0em , weight: "regular" , fill: rgb("#333333")); Max. 25-50],),
  table.hline(),
  table.cell(align: horizon + left)[Croissance du PIB#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[  0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,37~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,22~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,19~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,18~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,01~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Ecart de production#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,10~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,91~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,74~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,57~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,40~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −1,25~],
  table.cell(align: horizon + left)[Croissance du PIB potentiel#text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,15~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,04~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,02~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Inflation#text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]]], table.cell(align: horizon + right)[  1,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,82~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,76~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,74~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,57~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  1,52~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,57~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,72~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Taux de chômage#text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]]], table.cell(align: horizon + right)[  7,70~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,03~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,47~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,48~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,37~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  8,18~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,97~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  7,77~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  7,58~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  6,99~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  8,48~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Solde public#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −5,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,67~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,46~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −4,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,94~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,64~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,39~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,20~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,07~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −3,20~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[ −2,98~],
  table.cell(align: horizon + left)[Solde public primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,75~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,29~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,39~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,89~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,42~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,02~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,30~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,53~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,13~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,14~],
  table.cell(align: horizon + left)[Solde public structurel primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −2,85~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,13~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,31~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,80~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,37~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,02~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,33~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  0,57~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  0,73~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,13~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  1,13~],
  table.cell(align: horizon + left)[Impulsion budgétaire primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −1,06~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,83~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,43~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,31~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,23~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,16~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,01~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont PO#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,42~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,08~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[   dont dépense#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[ −0,09~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,42~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,25~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,19~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,16~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,12~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,08~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −0,00~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Cumul de l\'impulsion primaire#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[---], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −0,50~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,33~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −1,84~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,27~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,65~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −2,96~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[ −3,19~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −3,35~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[ −3,75~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[#text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]]~ −3,75~],
  table.cell(align: horizon + left, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[Taux souverain courant#text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]]], table.cell(align: horizon + right, stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,35~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,66~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,87~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,77~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,78~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,77~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,77~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,78~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[  3,79~], table.cell(align: horizon + right, fill: rgb("#ebebeb"), stroke: (top: (paint: rgb("#000000"), thickness: 0.75pt)))[---],
  table.cell(align: horizon + left)[Taux souverain pondéré#text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]]], table.cell(align: horizon + right)[  2,02~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,11~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,28~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,46~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,73~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,85~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,95~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,04~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,71~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[---],
  table.cell(align: horizon + left)[Dette publique#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[115,60~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[117,51~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[119,53~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[120,55~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,21~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,55~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,68~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[121,65~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[121,45~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[120,11~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[121,68~],
  table.cell(align: horizon + left)[Charge d\'intérêts#text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]]], table.cell(align: horizon + right)[  2,34~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,38~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,62~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  2,86~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,05~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,22~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,37~], table.cell(align: horizon + right, fill: rgb("#f2f2f2"))[  3,49~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  3,60~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  4,33~], table.cell(align: horizon + right, fill: rgb("#ebebeb"))[  4,39~],
  table.hline(),
  table.footer(table.cell(colspan: 12)[#block[
    #text(size: 0.75em , style: "italic" , weight: "regular")[#super[a]] évolution en % annuel, volume #text(size: 0.75em , style: "italic" , weight: "regular")[#super[b]] % du PIB #text(size: 0.75em , style: "italic" , weight: "regular")[#super[c]] Minimum #text(size: 0.75em , style: "italic" , weight: "regular")[#super[d]] déflateur du PIB, évolution en % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[e]] % de la population active #text(size: 0.75em , style: "italic" , weight: "regular")[#super[f]] 10~ans, % annuel #text(size: 0.75em , style: "italic" , weight: "regular")[#super[g]] % annuel
    ]],
    table.cell(colspan: 12)[Données AMECO printemps 2026, première année de simulation : 2026],
    table.cell(colspan: 12)[Simulation debtwatch v2.0 (github.com/OFCE/dwr), France, stochastique (256 tirages), valeurs moyennes],
    table.cell(colspan: 12)[],),
)}
], caption: figure.caption(
position: top, 
[
Ajustement optimal avec cible de dette à 120%, taux plus élevés
]), 
kind: "quarto-float-tbl", 
supplement: "Tableau", 
)
<tbl-taux>


#heading(level: 2, numbering: none)[Annexe 5 : Principes de la modélisation]
<annexe-5-principes-de-la-modélisation>
L'équation de retour à l'équilibre de l'écart de production est écrite comme un modèle à correction d'erreur (MCE). Les impulsions jouent sur l'écart de production. Le multiplicateur instantané est défini par le paramètre $o g_(d e p)$ ou $o g_(p o)$. Le multiplicateur complet (ou de long terme) dépend des autres paramètres du MCE, $o g_(l a g)$ et $o g_(m c e)$. Les valeurs utilisées pour les simulations sont : $o g_(d e p) = 0.8$, $o g_(p o) = 0.5$, $o g_(m c e) = 0.3$, $o g_(l a g) = 0.25$.

#math.equation(block: true, numbering: equation-numbering, [ $ Delta o g = o g_(l a g) times Delta o g_(- 1) - o g_(m c e) times o g + o g_(d e p) times i b_(d e p) + o g_(p o) times i b_(p o) + epsilon $ ])<eq-og>

Un bruit est ajouté, avec une autocorrélation. $o g n_sigma = 0.015$ et $o g n_(a r) = 0.9$.

$ Delta epsilon = o g n_(a r) times epsilon_(- 1) + epsilon.alt_(o g) \( tilde.op cal(N) \( 0 \, o g n_sigma^2 \) \) $

Le taux de croissance du prix du PIB (appelé inflation, on ne distingue pas prix de consommation ou autres), suit également un MCE. Le long terme est défini par la cible d'inflation et un effet de Phillips. Les paramètres sont $i n f_(l a g) = 0$, $i n f_(m c e) = 0.68$, $i n f_(t c h o) = 0.15$.

$ Delta dot(p) = i n f_(l a g) times d dot(p)_(- 1) - i n f_(m c e) times \( dot(p) - i n f_(s t a r) - i n f_(t c h o) times \( u - n a i r u \) \) + eta $

Comme pour l'écart de production, on ajoute une perturbation aléatoire. $i n f_sigma = 0.01$, $i n f n_(a r) = 0.0$.

$ Delta eta = i n f n_(a r) times eta_(- 1) + epsilon.alt_(i n f) \( tilde.op cal(N) \( 0 \, i n f_(s i g m a)^2 \) \) $

La dynamique de la dette suit une équation habituelle. $t_(a u t)$ assure l'équilibre comptable et prend en compte les recettes non fiscales.

$ Delta d e t t e = - s_p + i n t - t_(a u t) times v p i b + o n e_(o f f) $

Le taux de marché est défini comme la somme de l'inflation, de la croissance potentielle, d'une prime de risque $e c$ et d'un terme qui dépend de l'écart de la dette à une dette de référence $d^(* *)$. Dans les simulations $r_(d e t t e)$ est nul.

$ r_(i n s t) = g_(p o t) + dot(p) + e c + r_(d e t t e) times \( d - d^(* *) \) $

La prime de risque suit un MCE, $e c_(m c e) = 0.5$.

$ Delta e c = - e c_(m c e) times \( e c_(- 1) - e c^(*) \) $

Le taux moyen pondéré de la dette publique ou taux apparent est la moyenne entre le taux moyen pondéré antérieur et le taux courant, comme approximation de tranches de dettes de maturité égale et de différentes dates d'émission. $r_(m a t) = 9$.

$ r_(a p p) = 1 \/ r_(m a t) times r_(i n s t \, - 1) + \( 1 - 1 \/ r_(m a t) \) times r_(a p p \, - 1) $

La règle linéaire qui définit les impulsions est ($t p o_(s^(*)) = 0.25$, $t p o_(o g) = - 0.25$, $t p o_(d^(*)) = - 0.1$:

$ i b_(l r) = t p o_(s^(*)) times \( s p p - s^(*) \) + t p o_(d^(*)) times \( d - d^(*) \) + t p o_(o g) times o g $

Le chômage suit un MCE dont la cible est le nairu et qui est perturbé par l'écart de production, le coefficient d'Okun à court terme est définit par $t c h o_(o k u n) = 0.5$ et unitaire à long terme (i.e.~en cas de choc persistant sur l'écart de production). $t c h o_(m c e) = 0.27$.

$ Delta t c h o = - t c h o_(o k u n) times d o g - t c h o_(m c e) times \( t c h o + o g - n a i r u \) $

Le nairu s'ajuste progressivement à sa cible, afin de lisser les évolutions. $n a i r u_(m c e) = 0.2$

$ Delta n a i r u = - n a i r u_(m c e) times \( n a i r u_(- 1) - n a i r u^(*) \) $

Une politique budgétaire optimale est calculée : les impulsions budgétaires successives (disons de 2025 à 2055) sont déterminées de façon à ce que la dette publique soit égale à la cible de dette à partir d'une date donnée (dans le cas central 2045, 20~ans après 2025). Ce problème admet une infinité de solutions, puisque il suffit que le déficit primaire soit nul à partir de 2045 et que la dette en 2045 soit égale à la cible. On choisit parmi toutes ces solutions celle qui minimise l'écart quadratique d'activité ou des impulsions. Formellement, la fonction de perte s'écrit :

#math.equation(block: true, numbering: equation-numbering, [ $ cal(L) = lambda_(o g) sum_(t = 2025)^oo beta^t o g_t^2 + lambda_(i b) sum_(t = 2025)^oo beta^t i b_t^2 + lambda_(m a x) max_(t gt.eq 2025) \( - o g_t \) $ ])<eq-perte-quad>

$lambda_(o g)$, $lambda_(i b)$ et $lambda_(m a x)$ permettent de donner un poids relatif aux trois termes de cette expression. $beta$ est un facteur d'actualisation. Cette fonction de perte est permet l'utilisation d'algorithmes numériques standards de minimisation (en l'occurence COBYLA tel qu'implémenté dans #cite(<nloptr>, form: "prose")).

Le premier terme s'interprète la minimisation de la « variance » des écarts de production, c'est-à-dire une trajectoire qui s'écarte le moins possible de 0 et qui recherche des trajectoires où l'écart est le plus constant possible. On minimise ainsi l'impact de la politique restrictive. La partie en $i b_t^2$ est l'équivalent, mais pour les instruments. Elle permet de représenter un monde où il est difficile de modifier le budget. Cette inertie, si elle n'est pas rationnelle, est réaliste. Le mélange entre les deux fonctions de perte conduit à associer les deux arguments. Le facteur d'actualisation consiste à préférer des conséquences lointaines plutôt qu'immédiates et constitue une façon simple de justifier la #emph[backloading] par rapport au #emph[frontloading].

Enfin, le troisième terme conduit à prrendre en compte le creux d'écart de production. Cette fonction de perte conduit, comme on le verra plus loin, à produire des trajectoires dont la partie $o g_t$ négative est la plus limitée possible. Ce terme de la fonction de perte induit un #emph[backloading] assez fort, par construction. Pour faciliter le calcul numérique, on utilise une approximation continue de la fonction $m a x$#footnote[par une fonction $m a x \( x_1 \, x_2 \) approx \( x_1^sigma + x_2^sigma \)^(1 \/ sigma)$ avec $sigma$ assez grand (en pratique on prendra 8).].

#math.equation(block: true, numbering: equation-numbering, [ $ cal(L) = m a x \( - o g_t \) $ ])<eq-perte-min>

Le programme que l'on résout est alors donné par #ref(<eq-prog-min>, supplement: [équation]), $cal(S)$ étant la solution du système dynamique qui permet de calculer la trajectoire de la dette à partir des impulsions :

#math.equation(block: true, numbering: equation-numbering, [ $ min_(i b_t) cal(L) upright(" sous contrainte : pour ") t gt.eq T upright(", ") d_t = d^(*) upright("avec ") d_t = cal(S) \( i b_t \) $ ])<eq-prog-min>

#bibliography(("ajust.bib"))

