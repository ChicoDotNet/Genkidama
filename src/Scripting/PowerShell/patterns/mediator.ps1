Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

& {
    $events = [System.Collections.Generic.List[string]]::new()
    $colleagues = @{}

    $mediator = {
        param(
            [string]$Sender,
            [string]$Recipient,
            [string]$Message
        )

        if (-not $colleagues.ContainsKey($Recipient)) {
            throw "unknown colleague: $Recipient"
        }

        & $colleagues[$Recipient] $Sender $Message
    }

    $colleagues['inventory'] = {
        param([string]$Sender, [string]$Message)
        $events.Add("inventory<-${Sender}:${Message}")
    }
    $colleagues['payment'] = {
        param([string]$Sender, [string]$Message)
        $events.Add("payment<-${Sender}:${Message}")
    }

    $payment = { param([string]$Message) & $mediator 'payment' 'inventory' $Message }
    $inventory = { param([string]$Message) & $mediator 'inventory' 'payment' $Message }

    & $payment 'paid'
    & $inventory 'reserved'

    $expected = @('inventory<-payment:paid', 'payment<-inventory:reserved')
    if (($events -join ',') -ne ($expected -join ',')) {
        throw 'Mediator did not coordinate colleagues.'
    }

    try {
        & $mediator 'payment' 'unknown' 'ignored'
        throw 'Unknown colleague unexpectedly accepted.'
    }
    catch {
        if ($_.Exception.Message -notlike 'unknown colleague:*') {
            throw
        }
    }
}
