public class Nonthisfieldaccess {
  public A() {}
  public int x;
  public void m() {
    new A().x = 42;
  }
}