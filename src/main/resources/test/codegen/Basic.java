public class Basic {
  public int x;

  public Basic() {
    this.x = 3;
  }

  public static int test() {
    // boolean b = true;
    //boolean e = false;
    Basic a = new Basic();
    // boolean c = e || a instanceof Basic;
    //boolean d = b && a instanceof Basic;

    // if (c && d){
    //   return 123;
    // }
    // else {
    //   return 12378;
    // }
    return a.x;
  }
}
