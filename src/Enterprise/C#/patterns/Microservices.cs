namespace Genkidama.PatternExamples;
public static class MicroservicesExample { public static bool Run(){var stock=7;bool Reserve(int q){if(q>stock)return false;stock-=q;return true;}string Place(int q)=>Reserve(q)?"confirmed":"rejected";return Place(2)=="confirmed"&&stock==5;} }
