public class NotExpression {
    public static int test() {
        int ret = 0;
        if (!true) {
            ret = ret + 1;
        }

        if (!false) {
            ret = ret + 1;
        }

        return ret;
    }
}
