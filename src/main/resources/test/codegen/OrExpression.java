public class OrExpression {
    public static int test() {
        int ret = 0;
        if (true || true) {
            ret = ret + 1;
        }

        if (true || false) {
            ret = ret + 1;
        }

        if (false || true) {
            ret = ret + 1;
        }

        if (false || false) {
            ret = ret + 1;
        }

        return ret;
    }
}