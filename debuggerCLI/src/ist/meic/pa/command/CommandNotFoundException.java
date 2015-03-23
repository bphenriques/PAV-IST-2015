package ist.meic.pa.command;

public class CommandNotFoundException extends CommandException {

	private static final long serialVersionUID = 1L;
	
	private String commandNotFound;

	public CommandNotFoundException(String commandNotFound) {
		super();
		this.commandNotFound = commandNotFound;
	}
	
	@Override
	public String getMessage() {
		return "Inserted command " + commandNotFound + " not found.";
	}
	
}
