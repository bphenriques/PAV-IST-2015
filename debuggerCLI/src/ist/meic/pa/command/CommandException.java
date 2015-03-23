package ist.meic.pa.command;

public class CommandException extends Exception {

	private String errorMessage;
	
	public CommandException() {
		super();
	}
	
	public CommandException(String errorMessage) {
		this.errorMessage = errorMessage;
	}


	
	@Override
	public String toString() {
		return errorMessage;
	}
	
}
