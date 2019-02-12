public class Nestedblocks {
  public Nestedblocks() {}
  public int m() {
    int x = 42;
    int y = 87;
    { int z = x+y;
      return z;
    }
  }
}