package ist.meic.pa.command.exception;

import ist.meic.pa.command.Command;

public class InvalidCommandOnStaticException extends CommandException {
	
	private static final long serialVersionUID = 1L;

	private Command c;
	
	public InvalidCommandOnStaticException (Command c) {
		super();
	}
	
	@Override
	public String getMessage() {
		return "Invalid command " + c.getCommandName() + "on static method";
	}

}
