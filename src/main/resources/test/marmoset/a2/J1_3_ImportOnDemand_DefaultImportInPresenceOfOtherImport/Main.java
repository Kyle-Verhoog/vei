public class Main {
    public static int y = 3;
    public int x;

    public Main() {
        y = 10;
        this.x = 15;
    }

    public int basicMethod(int i, int y) {
        return 3;
    }

    public int basicStaticMethod(int i, int y) {
        return 3;
    }

    public static int test() {
        Main b = new Main();
        // int i = b.basicMethod(1, 2);
        int i = b.y;
        int j = b.x;
        return i + j;
    }
}
