package test;

class Simple {
	public static int  ba = 11;
	public String bb = "bb";
	private int bc  = 22;

	public static void foo2() {
		throw new RuntimeException("inside super foo");
	}

	protected void bar2() {
		throw new RuntimeException("inside super bar");
	}
}

public class TestModifiers extends Simple {

	public static int a = 1;
	public String b = "b";
	private int c = 2;

	public static void foo() {
		throw new RuntimeException("inside foo");
	}

	public void bar() {
		throw new RuntimeException("inside bar");
	}

	private void test() {
		foo();
		bar();
		foo2();
		bar2();

	}

	public static void main(String[] args) {
		new TestModifiers().test();
	}
}
