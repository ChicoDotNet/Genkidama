module EnterpriseFacadeExample
let run () =
    let createCrm id = $"crm:create:{id}"
    let openBilling id = $"billing:open:{id}"
    let onboard id = $"{createCrm id}>{openBilling id}"
    onboard 77 = "crm:create:77>billing:open:77"
