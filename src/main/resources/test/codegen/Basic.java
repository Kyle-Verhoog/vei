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


    System.out.println(o[6] instanceof String);
    int x = 5;
    // System.out.println(x instanceof int);
    // Object obj = new Object();
    String s = "test";
    System.out.println(s instanceof Object);
    System.out.println(s instanceof String);
    System.out.println(s instanceof Integer[]);

    Object obj = (Object)"test";
    // System.out.println(obj.equals((Object)"test"));

    if (!o[6].equals((Object)"foobar")) {
      return 5;
    }
    return 0;
  }
}
