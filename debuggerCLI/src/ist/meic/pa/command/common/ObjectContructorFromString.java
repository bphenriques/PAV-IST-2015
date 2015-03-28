package ist.meic.pa.command.common;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.ConstructorNotFoundException;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

public final class ObjectContructorFromString {
	
	private String typeField;
	private String inputText;
	
	public ObjectContructorFromString(String typeField, String inputText) {
		this.typeField = typeField;
		this.inputText = inputText;
	}

	public Object convert()
			throws CommandException {

		try {
			String text = inputText.trim();
			String[] tokens = text.split("(", 2);
			if (tokens.length < 2)
				throw new CommandException("");

			String objectName = tokens[0];
			String arguments = tokens[1];
			int lastPar = arguments.lastIndexOf(')');
			arguments = arguments.substring(0, lastPar - 1);
			String[] argumentTokens = arguments.split(",");

			Object instance = construct(inputText, objectName, argumentTokens);

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
		}
		return inputText;

	}

	private Object construct(String inputText, String objectName,
			String[] argumentTokens) throws ClassNotFoundException,
			ConstructorNotFoundException, InstantiationException,
			IllegalAccessException, InvocationTargetException {
		
		checkConstructorArguments(argumentTokens);
		
		Class<?> objectClass = Class.forName(objectName);
		Constructor<?>[] objectConstructors = objectClass.getConstructors();
		List<Constructor<?>> possibleObjectConstructors = new LinkedList<Constructor<?>>();

		for (Constructor<?> constructor : objectConstructors) {
			if (constructor.getParameterTypes().length == argumentTokens.length) {
				possibleObjectConstructors.add(constructor);
			}
		}

		if (possibleObjectConstructors.isEmpty())
			throw new ConstructorNotFoundException(objectName,
					argumentTokens);
		
		int selectedContructor = 0;
		if (possibleObjectConstructors.size() > 1) {
			selectedContructor = requestConstructorIndex(
					possibleObjectConstructors, inputText);
		}
		
		Constructor<?> constructor = possibleObjectConstructors.get(selectedContructor);
		Object instance = constructor.newInstance(null);
		return instance;
	}

	private Object[] checkConstructorArguments(String[] argumentTokens) {
		
		for(String argument: argumentTokens) {
			if(argument.contains("("))
				construct(a)
		}
		
	}

	private int requestConstructorIndex(
			List<Constructor<?>> possibleObjectConstructors, String inputText) {

		System.out.println("Multiple possible constructors found for " + inputText);
		System.out.println("Please select one of the following by inserting the respective index:");
		int i = 0;
		for (Constructor<?> constructor : possibleObjectConstructors) {
			i++;
			System.out.println(" " + i + " - " + constructor.toString().split(" ", 2));
		}
		
		
		@SuppressWarnings("resource")
		Scanner scanner = new Scanner(System.in);
		int index;
		while (true) {
			System.out.print("Select (1 - " + i + "):> ");
			System.out.flush();
			String indexString = scanner.nextLine();
			try {
				index =  new Integer(indexString);
				break;
			} catch (NumberFormatException e) {
				System.out.println("'" + indexString + "' is not a Integer. Please try again");
			}
		}
		
		return index;
	}

}
