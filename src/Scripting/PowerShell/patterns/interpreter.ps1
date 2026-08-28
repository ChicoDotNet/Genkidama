Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $grammar = @{ ADD = { param($a,$b) $a + $b }; MUL = { param($a,$b) $a * $b } }
  $tokens = 'ADD 2 3'.Split(' ')
  $result = & $grammar[$tokens[0]] ([int]$tokens[1]) ([int]$tokens[2])
  if ($result -ne 5) { throw 'Interpreter did not evaluate the grammar.' }
}