public class NotEqualsExpression {
    public static int test() {
        int ret = 0;

        // not equals
        if (1 != 2) {
            ret = ret + 1;
        }

        if (1 != 1) {
            ret = ret + 1;
        }
        // ret = 2

        if ("arst" != "arst") {
            ret = ret + 1;
        }

        if ("arst" != "wqfp") {
            ret = ret + 1;
        }
        // ret = 3

        if ('a' != 'a') {
            ret = ret + 1;
        }

        if ('a' != 'b') {
            ret = ret + 1;
        }
        // ret = 4

        if ('a' != 97) {
            ret = ret + 1;
        }

        if ('a' != 98) {
            ret = ret + 1;
        }
        // ret = 5

        if (true != false) {
            ret = ret + 1;
        }
        // ret = 6

        if (true != true) {
            ret = ret + 1;
        }

        if (false != false) {
            ret = ret + 1;
        }

        return ret;
    }
}
