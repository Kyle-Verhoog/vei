public class ArraySubTypeTestFail {
    public static int test() {
        int ret = 0;

        Integer[] integers = new Integer[10];
        Object[] objs = new Object[10];
        String[] strings = new String[10];

        strings = objs;

        return ret;
    }
}
