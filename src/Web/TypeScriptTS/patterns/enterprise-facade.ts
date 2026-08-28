function enterpriseFacadePattern(){const crm=(id:number)=>`crm:create:${id}`,billing=(id:number)=>`billing:open:${id}`;return `${crm(77)}>${billing(77)}`==='crm:create:77>billing:open:77'}
