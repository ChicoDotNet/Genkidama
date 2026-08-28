namespace Genkidama.PatternExamples;
public static class ClientServerExample { public static bool Run(){(int,string) Server(string k)=>k=="sku-1"?(200,"stock=7"):(404,"missing");return Server("sku-1")==((200,"stock=7"));} }
