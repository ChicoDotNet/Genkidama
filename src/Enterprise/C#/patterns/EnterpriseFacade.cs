namespace Genkidama.PatternExamples;
public static class EnterpriseFacadeExample { public static bool Run(){string Crm(int i)=>$"crm:create:{i}";string Billing(int i)=>$"billing:open:{i}";return $"{Crm(77)}>{Billing(77)}"=="crm:create:77>billing:open:77";} }
