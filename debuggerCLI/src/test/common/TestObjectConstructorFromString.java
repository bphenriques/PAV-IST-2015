package test.common;

import ist.meic.pa.command.common.ObjectContructorFromString;

public class TestObjectConstructorFromString {

	public static void main(String[] args) {
		ObjectContructorFromString ocfs = new ObjectContructorFromString(A.class, "test.common.A(2, test.common.B(3))");
		Object o = ocfs.convert();
		System.out.println(o);

	}

}
