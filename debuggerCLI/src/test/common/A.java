package test.common;

public class A {

	private int a;
	private B b;
	
	public A(int a, B b) {
		this.a = a;
		this.b = b;
	}
	
	public String toString() {
		return "" + a + " " + b;
	}
	
}
