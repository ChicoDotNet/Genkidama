Set-StrictMode -Version Latest
# Publish-Subscribe: publishers address topics; subscribers receive matching events.
$received=@();$topics=@{news=@({param($v)$script:received+=$v})};$topics.news|ForEach-Object {&$_ 'v1'};if($received[0]-ne'v1'){throw 'Publish-Subscribe failed'}
