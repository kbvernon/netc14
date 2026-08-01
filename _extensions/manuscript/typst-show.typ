#show: doc => article(
  title: [$title$],
$if(subtitle)$
  subtitle: [$subtitle$],
$endif$
  authors: (
$for(by-author)$
$if(it.name.literal)$
    ( name: [$it.name.literal$],
      affiliation: ($for(it.affiliations)$"$it.name$"$sep$, $endfor$) ),
$endif$
$endfor$
  ),
  date: [$date$],
  abstract: [$abstract$],
  keywords: [$for(keywords)$$it$$sep$, $endfor$],
  target_journal: [$target_journal$],
  correspondence: [$correspondence$],
$if(mainfont)$
  font: ("$mainfont$"),
$endif$
$if(fontsize)$
  fontsize: $fontsize$,
$endif$
  doc,
)