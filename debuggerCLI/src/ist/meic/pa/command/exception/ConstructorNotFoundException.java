package ist.meic.pa.command.exception;

//TODO
public class ConstructorNotFoundException extends CommandException {
	
	private static final long serialVersionUID = 1L;
	private final String objectClass;
	private final String[] parameters;
	
	
	public ConstructorNotFoundException(String objectClass, String[] parameters) {
		this.objectClass = objectClass;
		this.parameters = parameters;
	}


	public String getObjectClass() {
		return objectClass;
	}


	public String[] getParameters() {
		return parameters;
	}
	
	
}
