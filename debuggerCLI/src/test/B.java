package test;

public class B extends DadOfB{
	double b = 3.14;

	static String myString = "hello debugger";
	static final int bubu = 10;
	A a;
	
	public double bar(int x) {
		System.out.println("Inside B.bar");
		return (1 / x);
	}

	public double baz(Object x) {
		System.out.println("Inside B.baz");
		System.out.println(x.toString());
		return b;
	}

}
