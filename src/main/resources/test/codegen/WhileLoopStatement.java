public class WhileLoopStatement {
    public static int test() {
        int ret = 0;

        while(ret < -10) {
            ret = ret + 1;
        }

        while(ret < 100) {
            ret = ret + 1;
        }

        return ret;
    }
}