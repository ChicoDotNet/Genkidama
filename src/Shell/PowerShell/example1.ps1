# Abstract Factory
function Create-Button {
    param ([string]$theme)
    if ($theme -eq "dark") {
        Dark-Button
    } elseif ($theme -eq "light") {
        Light-Button
    }
}

function Create-Checkbox {
    param ([string]$theme)
    if ($theme -eq "dark") {
        Dark-Checkbox
    } elseif ($theme -eq "light") {
        Light-Checkbox
    }
}

# Concrete Products
function Dark-Button {
    Write-Output "Dark Button"
}

function Light-Button {
    Write-Output "Light Button"
}

function Dark-Checkbox {
    Write-Output "Dark Checkbox"
}

function Light-Checkbox {
    Write-Output "Light Checkbox"
}

# Usage
Create-Button "dark"
Create-Checkbox "light"
