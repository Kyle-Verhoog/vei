public class ModuloExpression {
    public static int test() {
        int ret = 10;
        ret = ret % 3;
        // ret = 1

        if ((ret + 4) % 5 == 0) {
            ret = ret + 1;
        }
        // ret = 2

        if (3 % 1 == 0) {
            ret = ret + 1;
        }
        // ret = 3

        if (8 % 3 == 2) {
            ret = ret + 1;
        }
        // ret = 4

        return ret;
    }
}

