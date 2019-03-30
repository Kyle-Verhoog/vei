public class ArrayCreationIndexing {
    public static int test() {
        int ret = 0;

        int[] myArray = new int[1];
        myArray[0] = 1;
        ret = ret + myArray[0];
        // ret = 1
/*
        myArray = new int[5];
        for (int i = 0; i < 5; i = i + 1) {
            myArray[i] = i + 1;
        }

        for (int i = 0; i < 5; i = i + 1) {
            //ret = ret + myArray[i];
        }
        // ret = 16
*/
        return ret;
    }
}

