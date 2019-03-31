public class ArraySubTypeTest {
    public static int test() {
        int ret = 0;

        Integer[] integers = new Integer[10];
        Object[] objs = new Object[10];
        String[] strings = new String[10];

        objs = strings;
        objs = integers;

        return ret;
    }
}
