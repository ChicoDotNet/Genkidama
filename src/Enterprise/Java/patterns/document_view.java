class PatternCell{static boolean run(){String d="abc";return d.length()==3&&d.getBytes().length==3;}public static void main(String[]a){if(!run())throw new AssertionError();}}
