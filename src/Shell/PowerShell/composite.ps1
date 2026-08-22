Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-FileLeaf {
    param([int]$Bytes)

    [pscustomobject]@{
        Kind = 'file'
        Size = { $Bytes }.GetNewClosure()
    }
}

function New-FolderComposite {
    param([object[]]$Children)

    [pscustomobject]@{
        Kind = 'folder'
        Size = {
            $total = 0
            foreach ($child in $Children) {
                $total += & $child.Size
            }
            $total
        }.GetNewClosure()
    }
}

$readme = New-FileLeaf 2
$docs = New-FolderComposite @((New-FileLeaf 3), (New-FileLeaf 5))
$root = New-FolderComposite @($readme, $docs)

Write-Output "leaf=$(& $readme.Size)"
Write-Output "docs=$(& $docs.Size)"
Write-Output "root=$(& $root.Size)"
