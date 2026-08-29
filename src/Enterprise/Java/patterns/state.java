class PatternCell{enum S{IDLE,RUNNING}static boolean run(){S s=S.IDLE;if(s==S.IDLE)s=S.RUNNING;return s==S.RUNNING;}public static void main(String[]a){if(!run())throw new AssertionError();}}
