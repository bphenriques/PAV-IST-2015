package ist.meic.pa.command.common.extension;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.ConstructorNotFoundException;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public final class ObjectContructorFromString {

	private static final Map<Class<?>, Class<?>> PRIMITIVES_TO_WRAPPERS = new HashMap<Class<?>, Class<?>>();

	private final Class<?> typeField;
	private String inputText;

	// safe because both Long.class and long.class are of type Class<Long>
	@SuppressWarnings("unchecked")
	private static <T> Class<T> wrap(Class<T> c) {
		return c.isPrimitive() ? (Class<T>) PRIMITIVES_TO_WRAPPERS.get(c) : c;
	}

	public ObjectContructorFromString(Class<?> typeField, String inputText) {
		this.typeField = typeField;
		this.inputText = inputText;
		PRIMITIVES_TO_WRAPPERS.put(boolean.class, Boolean.class);
		PRIMITIVES_TO_WRAPPERS.put(byte.class, Byte.class);
		PRIMITIVES_TO_WRAPPERS.put(char.class, Character.class);
		PRIMITIVES_TO_WRAPPERS.put(double.class, Double.class);
		PRIMITIVES_TO_WRAPPERS.put(float.class, Float.class);
		PRIMITIVES_TO_WRAPPERS.put(int.class, Integer.class);
		PRIMITIVES_TO_WRAPPERS.put(long.class, Long.class);
		PRIMITIVES_TO_WRAPPERS.put(short.class, Short.class);
		PRIMITIVES_TO_WRAPPERS.put(void.class, Void.class);

	}

	public Object convert() throws CommandException {

		try {
			String text = inputText.trim();
			String[] tokens;
			if (text.startsWith("\"")) {
				tokens = new String[] { text };
			} else
				tokens = text.split("\\(", 2);

			Class<?> objectClass;
			String arguments;
			if (tokens.length < 2) {
				if (typeField.isPrimitive())
					objectClass = wrap(typeField);
				else
					objectClass = typeField;
				arguments = tokens[0];
				inputText = objectClass.getName() + "(" + inputText + ")";
			} else {
				String objectName = tokens[0];
				objectClass = Class.forName(objectName);
				arguments = tokens[1];
			}
			String[] argumentTokens = { arguments };
			if (!text.endsWith("\"")) {
				int lastPar = arguments.lastIndexOf(')');
				if (lastPar != -1)
					arguments = arguments.substring(0, lastPar);
				argumentTokens = arguments.split(",");
			}
			Object instance = construct(objectClass, argumentTokens);

			return instance;
		} catch (ClassNotFoundException e) {
			throw new CommandException(e + "\nRemember to use full class name.");
		} catch (InstantiationException e) {

			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (CommandException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return inputText;

	}

	private Object construct(Class<?> objectClass, String[] argumentTokens)
			throws ClassNotFoundException, InstantiationException,
			IllegalAccessException, InvocationTargetException, CommandException {

		Object[] inputArguments = checkInputArguments(argumentTokens);

		Constructor<?>[] objectConstructors = objectClass.getConstructors();
		List<Constructor<?>> possibleObjectConstructors = new LinkedList<Constructor<?>>();

		objectConstructorLoop: for (Constructor<?> constructor : objectConstructors) {
			Class<?>[] parameterTypes = constructor.getParameterTypes();
			if (parameterTypes.length == argumentTokens.length) {
				for (int i = 0; i < parameterTypes.length; i++) {
					if (inputArguments[i] != null
							&& !parameterTypes[i].equals(inputArguments[i]
									.getClass())) {
						continue objectConstructorLoop;
					}
				}
				possibleObjectConstructors.add(constructor);
			}
		}

		if (possibleObjectConstructors.isEmpty())
			throw new ConstructorNotFoundException(objectClass, argumentTokens);

		int selectedContructor = 0;
		if (possibleObjectConstructors.size() > 1) {
			selectedContructor = requestConstructorIndex(
					possibleObjectConstructors, inputText);
		}

		Constructor<?> constructor = possibleObjectConstructors
				.get(selectedContructor);
		instanciateConstructorArguments(constructor, argumentTokens,
				inputArguments);

		Object instance = constructor.newInstance(inputArguments);
		return instance;
	}

	private void instanciateConstructorArguments(Constructor<?> constructor,
			String[] argumentTokens, Object[] inputArguments) {
		try {
			Class<?>[] parameterTypes = constructor.getParameterTypes();
			for (int i = 0; i < inputArguments.length; i++) {
				if (inputArguments[i] == null) {
					Class<?> paramClass = parameterTypes[i];
					if (parameterTypes[i].isPrimitive())
						paramClass = wrap(paramClass);
					Constructor<?> paramConstructor = paramClass
							.getConstructor(String.class);
					inputArguments[i] = paramConstructor
							.newInstance(argumentTokens[i]);

				}
			}

		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InstantiationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private Object[] checkInputArguments(String[] argumentTokens)
			throws CommandException {

		List<Object> arguments = new LinkedList<Object>();

		for (int i = 0; i < argumentTokens.length; i++) {
			String argument = argumentTokens[i];
			Object arg = null;
			if (!(argument.startsWith("\"") && argument.endsWith("\""))) {
				if (argument.contains("(")) {

					ObjectContructorFromString ocfs = new ObjectContructorFromString(
							typeField, argument);
					arguments.add(ocfs.convert());
				}
			} else {
				argumentTokens[i] = argument
						.substring(1, argument.length() - 1);
			}
			arguments.add(arg);
		}
		return arguments.toArray();

	}

	private int requestConstructorIndex(
			List<Constructor<?>> possibleObjectConstructors, String inputText) {

		System.out.println("Multiple possible constructors found for "
				+ inputText);
		System.out
				.println("Please select one of the following by inserting the respective index:");
		int i = 0;
		for (Constructor<?> constructor : possibleObjectConstructors) {
			i++;
			String constructorString = constructor.toString();
			constructorString = constructorString.split(" ", 2)[1];
			constructorString = constructorString.split("throws", 2)[0];

			System.out.println(" " + i + " - " + constructorString);
		}

		@SuppressWarnings("resource")
		Scanner scanner = new Scanner(System.in);
		int index;
		while (true) {
			System.out.print("Select (1 - " + i + "):> ");
			System.out.flush();
			String indexString = scanner.nextLine();
			try {
				index = new Integer(indexString);
				break;
			} catch (NumberFormatException e) {
				System.out.println("'" + indexString
						+ "' is not a Integer. Please try again");
			}
		}

		return index - 1;
	}

}
