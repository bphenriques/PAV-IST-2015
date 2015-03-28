package ist.meic.pa.command.exception;

//TODO
public class ConstructorNotFoundException extends CommandException {
	
	private static final long serialVersionUID = 1L;
	private final Class<?> objectClass;
	private final String[] parameters;
	
	
	public ConstructorNotFoundException(Class<?> objectClass, String[] parameters) {
		this.objectClass = objectClass;
		this.parameters = parameters;
	}


	public Class<?> getObjectClass() {
		return objectClass;
	}


	public String[] getParameters() {
		return parameters;
	}
	
	
}
