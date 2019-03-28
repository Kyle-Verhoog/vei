public class Boolean {
  public static int test() {
    int ret = 0;

    if (!true) {
      ret = 0;
    }

    if (true) {
      ret = ret + 1;
    }

    return ret;
  }
}
