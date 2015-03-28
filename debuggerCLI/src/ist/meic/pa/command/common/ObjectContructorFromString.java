package ist.meic.pa.command.common;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.ConstructorNotFoundException;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

public final class ObjectContructorFromString {

	private final Class<?> typeField;
	private final String inputText;

	public ObjectContructorFromString(Class<?> typeField, String inputText) {
		this.typeField = typeField;
		this.inputText = inputText;
	}

	public Object convert() {

		try {
			String text = inputText.trim();
			String[] tokens = text.split("\\(", 2);
			
			String objectName;
			String arguments;
			if (tokens.length < 2) {
				objectName=typeField.getName();
				arguments = tokens[0];
			} else {
				objectName = tokens[0];
				arguments = tokens[1];
			}

			int lastPar = arguments.lastIndexOf(')');
			if(lastPar!=-1)
				arguments = arguments.substring(0, lastPar - 1);
			String[] argumentTokens = arguments.split(",");

			Object instance = construct(objectName, argumentTokens);

			return instance;
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
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
		} catch (CommandException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return inputText;

	}

	private Object construct(String objectName, String[] argumentTokens)
			throws ClassNotFoundException, InstantiationException,
			IllegalAccessException, InvocationTargetException, CommandException {

		Object[] inputArguments = checkInputArguments(argumentTokens);

		Class<?> objectClass = Class.forName(objectName);
		Constructor<?>[] objectConstructors = objectClass.getConstructors();
		List<Constructor<?>> possibleObjectConstructors = new LinkedList<Constructor<?>>();

		for (Constructor<?> constructor : objectConstructors) {
			System.out.println(constructor.toString());
			Class<?>[] parameterTypes = constructor.getParameterTypes();
			if (parameterTypes.length == argumentTokens.length) {
				for (int i = 0; i < parameterTypes.length; i++) {
					if (inputArguments[i] != null
							&& !parameterTypes[i].equals(inputArguments[i]
									.getClass())) {
						continue;
					}
					possibleObjectConstructors.add(constructor);
				}
			}
		}

		if (possibleObjectConstructors.isEmpty())
			throw new ConstructorNotFoundException(objectName, argumentTokens);

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
					Constructor<?> paramConstructor = paramClass.getConstructor(
							String.class);
					inputArguments[i] = paramConstructor.newInstance(argumentTokens[i]);
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

		for (String argument : argumentTokens) {
			if (argument.contains("(")) {

				ObjectContructorFromString ocfs = new ObjectContructorFromString(
						typeField, argument);
				arguments.add(ocfs.convert());
			} else {
				arguments.add(null);
			}
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
			System.out.println(" " + i + " - "
					+ constructor.toString().split(" ", 2)[1]);
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

		return index;
	}

}
