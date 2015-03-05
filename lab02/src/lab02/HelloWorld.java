package lab02;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class HelloWorld implements Message {

	public static void main(String[] args) {

		String inputCommand;
		BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

		Class<?> requestedClass = null;
		Object result = null;
		
		Map<String, Class<?>> map = new HashMap<String, Class<?>>();

		while (true) {
			try {
				System.out.printf("Command:> ").flush();
				inputCommand = br.readLine();

				String[] parsedString = inputCommand.split(" ");

				if (parsedString.length == 0) {
					System.out.println("No command given");
					continue;
				}

				String command = parsedString[0];
				if (command.equals("Class")) {
					requestedClass = Class.forName(parsedString[1]);
					System.out.println(requestedClass);
				} else if (command.equals("Set")) {

					map.put(parsedString[1], requestedClass);
					System.out.println("Saved name for object of type: "
							+ requestedClass);
				} else if (command.equals("Get")) {
					requestedClass = map.get(parsedString[1]);
					System.out.println(requestedClass);
				} else if (command.equals("Index")) {
					
					if(result.getClass().isArray()){
						Object[] array = (Object[]) result;
						array[(new Integer(parsedString[1])).intValue()];
					}
					
				} else {
					
					if (requestedClass != null) {
						Method[] methods = requestedClass.getDeclaredMethods();

						List<Method> sameNameMethods = new ArrayList<Method>();

						for (Method m : methods) {
							if (m.getName().equals(parsedString[0])) {
								sameNameMethods.add(m);
							}
						}

						for (Method m : sameNameMethods) {
							if (m.getParameterTypes().length != parsedString.length - 1) {
								sameNameMethods.remove(m);
							}
						}

						List<Object> arguments = new ArrayList<Object>();

						
						boolean success = false;
						for (Method m : sameNameMethods) {
							Class<?> parameterTypes[] = m.getParameterTypes();

							try {
								for (int i = 1; i < parsedString.length; i++) {
									Constructor<?> c = parameterTypes[i - 1]
											.getConstructor(String.class);
									Object b = c.newInstance(parsedString[i]);
									arguments.add(b);
								}
								Object obj = requestedClass.newInstance();
								System.out.println("Applying command " + parsedString[0]);
								result = m.invoke(obj, arguments.toArray());
								System.out.println(result);
								success = true;
								break;
							} catch (IllegalAccessException e) {
								e.printStackTrace();
							} catch (IllegalArgumentException e) {
								e.printStackTrace();
							} catch (InvocationTargetException e) {
								e.printStackTrace();
							}
						}
						if (success) {
							System.out.println("SUCESS");
						}else{
							System.out.println("FAILLL");
							
						} 

					}

				}

			} catch (IOException e) {
				System.err.println("Rebentou");
			} catch (ClassNotFoundException e) {
				System.err.println("Deu erro, chama o prof");
			} catch (NoSuchMethodException e) {
				e.printStackTrace();
			} catch (SecurityException e) {
				e.printStackTrace();
			} catch (InstantiationException e) {
				e.printStackTrace();
			}

		}

	}

	@Override
	public void say() {
		System.out.println("Hello World!");
	}

}
