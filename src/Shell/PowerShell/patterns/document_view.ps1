Set-StrictMode -Version Latest
# Document-View: multiple views project the same document differently.
$document=@{title='One'};$a={$script:document.title};$b={$script:document.title.ToUpperInvariant()};if("$(&$a)|$(&$b)"-ne'One|ONE'){throw 'Document-View failed'}
