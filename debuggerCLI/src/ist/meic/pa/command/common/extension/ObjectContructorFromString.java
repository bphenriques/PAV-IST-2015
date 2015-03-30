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

/**
 * The Class ObjectContructorFromString creates an instance from a desired type
 * or uses a constructor if the user so desires. Ex String(2)
 */
public final class ObjectContructorFromString {

	/** The Constant PRIMITIVES_TO_WRAPPERS. */
	private static final Map<Class<?>, Class<?>> PRIMITIVES_TO_WRAPPERS = new HashMap<Class<?>, Class<?>>();

	/** The type field. */
	private final Class<?> _typeField;

	/** The input text. */
	private String _inputText;

	/**
	 * Wrap.
	 *
	 * @param <T>
	 *            the generic type
	 * @param c
	 *            the c
	 * @return the class
	 */
	
	// safe because both Long.class and long.class are of type Class<Long>
	@SuppressWarnings("unchecked")
	private static <T> Class<T> wrap(Class<T> c) {
		return c.isPrimitive() ? (Class<T>) PRIMITIVES_TO_WRAPPERS.get(c) : c;
	}

	/**
	 * Instantiates a new object contructor from string.
	 *
	 * @param typeField
	 *            the type field
	 * @param inputText
	 *            the input text
	 */
	public ObjectContructorFromString(Class<?> typeField, String inputText) {
		this._typeField = typeField;
		this._inputText = inputText;
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

	/**
	 * Convert turns the String into either a primitive type instance or a specified object instance.
	 *
	 * @return the object
	 * @throws CommandException
	 *             the command exception
	 */
	public Object convert() throws CommandException {

		try {
			String text = _inputText.trim();
			String[] tokens;
			if (text.startsWith("\"")) {
				tokens = new String[] { text };
			} else
				tokens = text.split("\\(", 2);

			Class<?> objectClass;
			String arguments;
			if (tokens.length < 2) {
				if (_typeField.isPrimitive())
					objectClass = wrap(_typeField);
				else
					objectClass = _typeField;
				arguments = tokens[0];
				_inputText = objectClass.getName() + "(" + _inputText + ")";
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
				
				argumentTokens = commaTokenizer(arguments);
			}
			
			
			Object instance = construct(objectClass, argumentTokens);

			return instance;
		} catch (ClassNotFoundException e) {
			throw new CommandException(e + "\nRemember to use full class name.");
		} catch (Exception e) {
			e.printStackTrace();
			throw new CommandException(e.getMessage());
		}

	}

	/**
	 * 
	 * @param arguments
	 * @return
	 */
	private String[] commaTokenizer(String arguments) {
		int bracketCounter = 0;
		List<String> argTok = new LinkedList<String>();
		StringBuilder buffer=new StringBuilder();
		for (char c: arguments.toCharArray()) {
			if (c=='(') bracketCounter++;
		    if (c==')') bracketCounter--;
		    if (c==',' && bracketCounter==0) {
		        argTok.add(buffer.toString());
		        buffer = new StringBuilder();
		    } else { 
		        buffer.append(c);
		    }
		}
		argTok.add(buffer.toString());
		return argTok.toArray(new String[0]);
	}

	/**
	 * Construct.
	 *
	 * @param objectClass
	 *            the object class
	 * @param argumentTokens
	 *            the argument tokens
	 * @return the object
	 * @throws ClassNotFoundException
	 *             the class not found exception
	 * @throws InstantiationException
	 *             the instantiation exception
	 * @throws IllegalAccessException
	 *             the illegal access exception
	 * @throws InvocationTargetException
	 *             the invocation target exception
	 * @throws CommandException
	 *             the command exception
	 * @throws NoSuchMethodException
	 *             the no such method exception
	 * @throws SecurityException
	 *             the security exception
	 * @throws IllegalArgumentException
	 *             the illegal argument exception
	 */
	private Object construct(Class<?> objectClass, String[] argumentTokens)
			throws ClassNotFoundException, InstantiationException,
			IllegalAccessException, InvocationTargetException,
			CommandException, NoSuchMethodException, SecurityException,
			IllegalArgumentException {

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
					possibleObjectConstructors, _inputText);
		}

		Constructor<?> constructor = possibleObjectConstructors
				.get(selectedContructor);

		instantiateConstructorArguments(constructor, argumentTokens,
				inputArguments);

		Object instance = constructor.newInstance(inputArguments);
		return instance;
	}

	/**
	 * Instantiate constructor arguments.
	 *
	 * @param constructor
	 *            the constructor
	 * @param argumentTokens
	 *            the argument tokens
	 * @param inputArguments
	 *            the input arguments
	 * @throws NoSuchMethodException
	 *             the no such method exception
	 * @throws SecurityException
	 *             the security exception
	 * @throws InstantiationException
	 *             the instantiation exception
	 * @throws IllegalAccessException
	 *             the illegal access exception
	 * @throws IllegalArgumentException
	 *             the illegal argument exception
	 * @throws InvocationTargetException
	 *             the invocation target exception
	 */
	private void instantiateConstructorArguments(Constructor<?> constructor,
			String[] argumentTokens, Object[] inputArguments)
			throws NoSuchMethodException, SecurityException,
			InstantiationException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {

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

	}

	/**
	 * Check input arguments.
	 *
	 * @param argumentTokens
	 *            the argument tokens
	 * @return the object[]
	 * @throws CommandException
	 *             the command exception
	 */
	private Object[] checkInputArguments(String[] argumentTokens)
			throws CommandException {

		List<Object> arguments = new LinkedList<Object>();

		for (int i = 0; i < argumentTokens.length; i++) {
			String argument = argumentTokens[i];
			Object arg = null;
			if (!(argument.startsWith("\"") && argument.endsWith("\""))) {
				if (argument.contains("(")) {

					ObjectContructorFromString ocfs = new ObjectContructorFromString(
							_typeField, argument);
					arg = ocfs.convert();
				}
			} else {
				argumentTokens[i] = argument
						.substring(1, argument.length() - 1);
			}
			arguments.add(arg);
		}

		return arguments.toArray();

	}

	/**
	 * Request constructor index.
	 *
	 * @param possibleObjectConstructors
	 *            the possible object constructors
	 * @param inputText
	 *            the input text
	 * @return the int
	 */
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
