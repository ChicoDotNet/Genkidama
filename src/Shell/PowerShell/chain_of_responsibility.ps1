$visited = [System.Collections.Generic.List[string]]::new()
$handlers = @(
    [pscustomobject]@{ Name = 'faq'; Limit = 50 },
    [pscustomobject]@{ Name = 'billing'; Limit = 500 },
    [pscustomobject]@{ Name = 'escalation'; Limit = $null }
)

function Invoke-RefundChain([int]$Amount) {
    foreach ($handler in $handlers) {
        $visited.Add($handler.Name)
        if ($null -eq $handler.Limit -or $Amount -le $handler.Limit) {
            return [pscustomobject]@{
                Handled = $handler.Name
                Result = "refund($Amount)"
            }
        }
    }
    throw 'unhandled request'
}

$outcome = Invoke-RefundChain 250
"visited=$($visited -join '>');handled=$($outcome.Handled);result=$($outcome.Result)"
