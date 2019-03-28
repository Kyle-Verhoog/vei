public class GreaterThanExpression {
    public static int test() {
        int ret = 0;

        if (1 > -1) {
            ret = ret + 1;
        }

        if (1 > 0) {
            ret = ret + 1;
        }

        if (1 > 1) {
            ret = ret + 1;
        }

        if (1 > 2) {
            ret = ret + 1;
        }

        return ret;
    }
}