Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

class RemoteDocumentStore {
    [int] $FetchCount = 0

    [string] Get([int] $Id) {
        $this.FetchCount++
        return "doc($Id)"
    }
}

class DocumentStoreProxy {
    hidden [RemoteDocumentStore] $Backend
    hidden [hashtable] $Cache = @{}

    [string] Get([int] $Id) {
        if ($this.Cache.ContainsKey($Id)) {
            return [string]$this.Cache[$Id]
        }
        if ($null -eq $this.Backend) {
            $this.Backend = [RemoteDocumentStore]::new()
        }
        $value = $this.Backend.Get($Id)
        $this.Cache[$Id] = $value
        return $value
    }

    [int] BackendCount() { return $(if ($null -eq $this.Backend) { 0 } else { 1 }) }
    [int] FetchCount() { return $(if ($null -eq $this.Backend) { 0 } else { $this.Backend.FetchCount }) }
}

$store = [DocumentStoreProxy]::new()
$first = $store.Get(42)
$second = $store.Get(42)
"backend=$($store.BackendCount());fetches=$($store.FetchCount());first=$first;second=$second"
