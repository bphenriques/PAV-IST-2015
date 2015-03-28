package test.common;

import ist.meic.pa.command.common.ObjectContructorFromString;

public class TestObjectConstructorFromString {

	public static void main(String[] args) {
		ObjectContructorFromString ocfs = new ObjectContructorFromString(Integer.class, "2");
		ocfs.convert();

	}

}
