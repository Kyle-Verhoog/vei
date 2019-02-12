public class Closestmatchconstructoroverloading {
  public Closestmatchconstructoroverloading() {}
  public Closestmatchconstructoroverloading(Object x, Object y) {}
  public Closestmatchconstructoroverloading(Object x, A y) {}
  public void m() {
    new A(new A(), new A());
  }
}