public class Main {
    public Main() {

    }

    public int basicMethod(int i, int y) {
        return 3;
    }

    public int basicStaticMethod(int i, int y) {
        return 3;
    }

    public static int test() {
        Main b = new Main();
        int i = b.basicMethod(1, 2);
        return i;
    }
}
