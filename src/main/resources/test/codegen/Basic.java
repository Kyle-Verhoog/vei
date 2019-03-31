public class Basic {
  public static int y = 3;
  public int x;

  public Basic() {
    y = 10;
    // this.x = 15;
  }

  public int basicMethod(int i, int y) {
    return i + y;
  }

  public static int basicStaticMethod(int i, int j) {
    return i + j;
  }

  public static int test() {
    // Basic b = new Basic();
    // Basic e = new Basic();
    // Basic e = new Basic();
    // int i = b.basicMethod(1, 2);
    int i = basicStaticMethod(1, 2);
    // int i = b.y;
    // int j = b.x;
    return i;
  }
}
