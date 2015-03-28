package test;

public class Example3 {
	public static void main(String[] args) {
		A b = new B().getA();
		System.out.println(b.foo(new B()));
	}
}
