package test;

public class A{
	int a = 1;
	int v = 69;
	
	A me = null;
	
	public A() {
		super();
	}
	
	public A(int a) {
		this.a=a;
	}
	
	public A(A as) {
		this.a=as.a;
	}
	
	public A(int v, A a){
		this.v = v;
		this.me = a;
	}
	
	public A(A a, int v){
		this.v = v;
		this.me = a;
	}
	
	public double foo(B b) {
		System.out.println("Inside A.foo");
		if (a == 1) {
			return b.bar(0, 10, "asdasd");
		} else {
			return b.baz(null);
		}
	}
	
	public String toString() {
		return "A.a: " + a;
	}
}
