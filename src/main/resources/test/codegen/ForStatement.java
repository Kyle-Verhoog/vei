public class ForStatement {
    public static int test() {
        int ret = 0;

        for (int i = 0; i <= 3; i = i + 1) {
            ret = ret + i;
        }
        // ret = 6

        for (int i = 3; i > -1; i = i - 1) {
            ret = ret + i;
        }
        // ret = 12

        return ret;
    }
}
