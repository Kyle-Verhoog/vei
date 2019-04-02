public class Basic {
  public Basic(int x) {
    // y = x;
  }

  // public void updateX() {
  //   x = 5;
  // }

  public static int test() {
    String[] o = new String[8];

    o[6] = "foobar";

    System.out.println(o[6]);


    if (!o[6].equals((Object)"foobar")) {
      return 5;
    }
    return 0;
  }
}
