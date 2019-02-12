// FAIL
// Java 1.3
public class Break {
  public int m(int x) {
    while (x>0) {
       x=x-1;
       if (x=42) break;
    }
    return x;
  }
}