public class Implicitthisformethods {
  public Implicitthisformethods() {}
  public int m1() {
    return 42;
  }
  public int m2() {
    return m1();
  }
}