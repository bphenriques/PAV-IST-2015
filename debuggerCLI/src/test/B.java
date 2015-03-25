package test;

public class B {
	double b = 3.14;

	static String myString = "hello debugger";
	static int bubu = 10;
	
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
