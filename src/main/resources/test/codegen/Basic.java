public class Basic {
  public static int y = 3;
  public int x;

  public Basic(int x, int z) {
    y = x + z;
    // this.x = 15;
  }

  // public int basicMethod(int i, int y) {
  //   return i + y;
  // }

  public static int test1(int KYLEPARAM1, int KYLEPARAM2) {
    int KYLE1 = 0;
    int KYLE2 = 1;
    int KYLE3 = 2;
    int KYLE4 = 3;
    return KYLEPARAM1 + KYLE2 + KYLEPARAM2;
  }

  public static int test2(int i) {
    return i;
  }

  public static int test3() {
    return 8;
  }

  public static int test() {
    Basic b = new Basic(3, 6);
    Basic e = new Basic(1, 7);
    Basic f = new Basic(4, 5);
    // int j = test2(3);
    // int k = test3();
    // int j = test3();
    return b.y;
  }
}
