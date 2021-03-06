package ist.meic.pa.command.exception;

/**
 * The Class ConstructorNotFoundException. It is thrown when a constructor was not found
 */
public class ConstructorNotFoundException extends CommandException {
	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;
	
	/** The object class. */
	private final Class<?> _objectClass;
	
	/** The parameters. */
	private final String[] _parameters;
	
	
	/**
	 * Instantiates a new constructor not found exception.
	 *
	 * @param objectClass the object class
	 * @param parameters the parameters
	 */
	public ConstructorNotFoundException(Class<?> objectClass, String[] parameters) {
		this._objectClass = objectClass;
		this._parameters = parameters;
	}


	/**
	 * Gets the object class.
	 *
	 * @return the object class
	 */
	public Class<?> getObjectClass() {
		return _objectClass;
	}


	/**
	 * Gets the parameters.
	 *
	 * @return the parameters
	 */
	public String[] getParameters() {
		return _parameters;
	}
	
	
}