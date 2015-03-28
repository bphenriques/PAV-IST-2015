package test.common;

import ist.meic.pa.command.common.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;

public class TestObjectConstructorFromString {

	public static void main(String[] args) throws CommandException {
		ObjectContructorFromString ocfs = new ObjectContructorFromString(A.class, "test.common.A(2, test.common.B(3))");
		Object o = ocfs.convert();
		System.out.println(o);

	}

}
