class PatternCell{static boolean run(){int s=7,snap=s;s=11;if(s!=11)return false;s=snap;return s==7;}public static void main(String[]a){if(!run())throw new AssertionError();}}
