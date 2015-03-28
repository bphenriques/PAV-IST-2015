package ist.meic.pa.debugger.stack;

/**
 * The Class StackElement represents a method call in the user code.
 */
public class StackElement {

	/** The arguments. */
	private final Object[] _arguments;

	/** The method name. */
	private final String _methodName;

	/** The invoking class. */
	private final Class<?> _invokingClass;

	/** The parameter types. */
	private Class<?> _parameterTypes[] = null;

	/** The return type. */
	private Class<?> _returnType = null;

	/**
	 * Instantiates a new stack element.
	 *
	 * @param invokingClass
	 *            the invoking class
	 * @param methodName
	 *            the method name
	 * @param returnType
	 *            the return type
	 * @param arguments
	 *            the arguments
	 */
	public StackElement(Class<?> invokingClass, String methodName,
			Class<?> returnType, Object... arguments) {
		super();
		this._methodName = methodName;
		this._invokingClass = invokingClass;
		this._arguments = arguments;
		this._returnType = returnType;
	}

	/**
	 * Gets the return type.
	 *
	 * @return the return type
	 */
	public Class<?> getReturnType() {
		return _returnType;
	}

	/**
	 * Sets the parameters types.
	 *
	 * @param parameterTypes
	 *            the new parameters types
	 */
	public void setParametersTypes(Class<?>... parameterTypes) {
		this._parameterTypes = parameterTypes;
	}

	/**
	 * Gets the arguments.
	 *
	 * @return the arguments
	 */
	public Object[] getArguments() {
		return _arguments;
	}

	/**
	 * Gets the method name.
	 *
	 * @return the method name
	 */
	public String getMethodName() {
		return _methodName;
	}

	/**
	 * Gets the ivoking class.
	 *
	 * @return the ivoking class
	 */
	public Class<?> getIvokingClass() {
		return _invokingClass;
	}

	/**
	 * Gets the parameter types.
	 *
	 * @return the parameter types
	 */
	public Class<?>[] getParameterTypes() {
		return _parameterTypes;
	}
}
