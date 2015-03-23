package ist.meic.pa.command;

public class CommandException extends Exception {

	private static final long serialVersionUID = -7148502222484812748L;
	private String errorMessage;
	
	public CommandException() {
		super();
	}
	
	public CommandException(String errorMessage) {
		this.errorMessage = errorMessage;
	}


	
	@Override
	public String getMessage() {
		return errorMessage;
	}
	
}
