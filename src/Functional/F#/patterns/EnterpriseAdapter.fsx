module EnterpriseAdapterExample

type Legacy = { Code: int; Cents: int }
type Canonical = { Id: int; Amount: float }

let run () =
    let legacy = { Code = 17; Cents = 1250 }
    let canonical = { Id = legacy.Code; Amount = float legacy.Cents / 100.0 }
    canonical.Id = 17 && canonical.Amount = 12.5
