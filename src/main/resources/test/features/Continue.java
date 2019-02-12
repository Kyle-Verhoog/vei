// FAIL
// Java 1.3
public class Continue {
  public int m(int x) {
    while (x>0) {
       x=x-1;
       if (x=87) {
         x=42;
         continue;
       }
    }
    return x;
  }
}