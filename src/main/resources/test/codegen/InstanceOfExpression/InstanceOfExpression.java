public class InstanceOfExpression {
    public static int test() {
        int ret = 0;

        // B is a subclass of A
        A a = new A();
        B b = new B();
        C c = new C();
        A ab = new B();

        if (a instanceof A) {
            ret = ret + 1;
        }
        // ret = 1

        if (a instanceof B || a instanceof C) {
            ret = ret + 1;
        }
        // ret = 1

        if (b instanceof A) {
            ret = ret + 1;
        }
        // ret = 2

        if (b instanceof B) {
            ret = ret + 1;
        }
        // ret = 3

        if (b instanceof C) {
            ret = ret + 1;
        }
        // ret = 3

        if (c instanceof A | c instanceof B) {
            ret = ret + 1;
        }
        // ret = 3

        if (ab instanceof A) {
            ret = ret + 1;
        }
        // ret = 4

        if (ab instanceof B) {
            ret = ret + 1;
        }
        // ret = 5

        if (ab instanceof C) {
            ret = ret + 1;
        }
        // ret = 5

        return ret;
    }
}