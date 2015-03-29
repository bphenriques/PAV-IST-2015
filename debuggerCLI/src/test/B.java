package test;

public class B extends DadOfB{
	double b = 3.14;

	static String myString = "hello debugger";
	static final int bubu = 10;
	private A a;
	
	public double bar(int x, int a, String test) {
		System.out.println("Inside B.bar with: " + a + " and " + test);
		return (1 / x);
	}
	
	public double bar2(int x) {
		System.out.println("ALTERNATIVE BAR");
		return 123432;
	}

	public double baz(Object x) {
		System.out.println("Inside B.baz");
		System.out.println(x.toString());
		return b;
	}

	
	public A getA() {
		if(a==null)
			throw new RuntimeException();
		return a;
	}
}
