Set-StrictMode -Version Latest
# Strategy: interchangeable algorithms are supplied to the same context.
$choose={param($values,$strategy)&$strategy $values};$min=&$choose @(3,1,2) {param($v)($v|Measure-Object -Minimum).Minimum};$max=&$choose @(3,1,2) {param($v)($v|Measure-Object -Maximum).Maximum};if($min-ne1-or$max-ne3){throw 'Strategy failed'}
