public class ArrayFail1 {
    public static int test() {
        int ret = 0;

        int[] myArray = new int[1];
        myArray[2 + -3] = 1;
        return ret;
    }
}

