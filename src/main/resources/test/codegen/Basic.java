public class Basic {
  public Basic(int x) {
    // y = x;
  }

  // public void updateX() {
  //   x = 5;
  // }

  public static int test() {
    // System.out.println(12345);
    /*
    int i = 12345;
    char[] ret = new char[15];
    int j = 0;
    boolean wasneg = false;
    if(i<0) {
      wasneg = true;
      i = -i;
    }
    if(i == 0) {
      ret[j] = '0';
      j = j + 1;
    } else {
      while(i > 0) {
        int d = i % 10;
        i = i / 10;
        ret[j] = (char) (d + '0');
        j = j + 1;
      }
    }
    if(wasneg) {
      ret[j] = '-';
      j = j + 1;
    }
    char[] ret2 = new char[j];
    for(i = 0; i < j; i = i + 1) ret2[i] = ret[j-1-i];
    */
    String x = "test";
    System.out.println(x.length());
    System.out.println(x.getchars()[0]);
    System.out.println(x.getchars()[1]);
    System.out.println(x.getchars()[2]);
    System.out.println(x.getchars()[3]);
    System.out.println("hello kevin\n");
    return 0; // x.charAt(1);
  }
}
