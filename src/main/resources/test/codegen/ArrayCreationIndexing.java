public class Basic {
    public static int test() {
        ret = 0;

        int[] myArray = new int[1];
        myArray[0] = 1;
        ret = ret + myArray[0];
        // ret = 1

        int[] myArray = new int[5];
        for (int i = 0; i < 5; i++) {
            myArray[i] = i + 1;
        }

        for (int i = 0; i < 5; i++) {
            ret = ret + myArray[i];
        }
        // ret = 16

        return ret;
    }
}
