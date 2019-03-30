public class Basic {
  public static int y = 3;
  public static int z = y + 2;

  public Basic() {

  }

  public int basicMethod(int i, int y) {
    return 3;
  }

  public int basicStaticMethod(int i, int y) {
    return 3;
  }

  public static int test() {
    // Basic b = new Basic();
    // int i = b.basicMethod(1, 2);
    return z;
  }
}
