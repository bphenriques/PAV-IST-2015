package test;

public class A{
	int a = 1;
	
	public A() {
		super();
	}
	
	public A(int a) {
		this.a=a;
	}
	
	public A(A as) {
		this.a=as.a;
	}
	
	public double foo(B b) {
		System.out.println("Inside A.foo");
		if (a == 1) {
			return b.bar(0);
		} else {
			return b.baz(null);
		}
	}
	
	public String toString() {
		return "A.a: " + a;
	}
}
