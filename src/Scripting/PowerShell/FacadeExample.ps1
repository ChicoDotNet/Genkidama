Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Invoke-Authentication([string] $User) { "auth($User)" }
function Invoke-Reservation([string] $Sku) { "reserve($Sku)" }
function Invoke-Charge([int] $Cents) { "charge($Cents)" }

function Invoke-Checkout([string] $User, [string] $Sku, [int] $Cents) {
    $auth = Invoke-Authentication $User
    $inventory = Invoke-Reservation $Sku
    $billing = Invoke-Charge $Cents
    "checkout=$auth>$inventory>$billing"
}

Invoke-Checkout 'alice' 'SKU-42' 499
