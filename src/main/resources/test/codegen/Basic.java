public class Basic {
  public static int y = 0;

  public Basic(int x) {
    y = x;
  }

  public static int test() {
    Basic b = new Basic(3);
    // System.out.println("test");
    return b.y;
  }
}
