package lab03;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

public class RunTests {

	static Map<String, Method> setupMap = new HashMap<String, Method>();

	public static void main(String[] args) {

		String classTestName = args[0];
		System.out.println("Class to Test: " + classTestName);
		try {
			Class classTestClass = Class.forName(classTestName);
			Method[] methods = classTestClass.getDeclaredMethods();
			//TODO: Recursivamente procurar nos pais da classe e acumular metodos
			for (Method m : methods) {

				if (m.isAnnotationPresent(Setup.class)) {

					Setup a = (Setup) m.getAnnotation(Setup.class);
					String name = a.value();

					setupMap.put(name, m);
				}

			}

			for (Method m : methods) {
				if (m.isAnnotationPresent(Test.class)) {

					Test a = (Test) m.getAnnotation(Test.class);
					String[] methodNames = a.value();

					if (methodNames[0].equals("*")) {
						methodNames = setupMap.keySet().toArray(
								new String[setupMap.size()]);
					}

					try {
						for (String mName : methodNames) {

							System.out.println("Calling: " + mName);

							Method methodToRun = setupMap.get(mName);
							methodToRun.setAccessible(true);
							methodToRun.invoke(null, null);
						}

						m.setAccessible(true);
						m.invoke(null, null);
						System.out.println("Test " + m + " OK!");
					} catch (Throwable t) {
						System.out.println("Test " + m + " failed");
					}
				}
			}

		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
}
