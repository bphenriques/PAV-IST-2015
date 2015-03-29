package ist.meic.pa.command.exception;


/**
 * The Class CommandNotFoundException.
 */
public class CommandNotFoundException extends CommandException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;
	
	/** The command that wasn't found. */
	private String _commandNotFound;

	/**
	 * Instantiates a new command not found exception.
	 *
	 * @param commandNotFound the command not found
	 */
	public CommandNotFoundException(String commandNotFound) {
		super();
		this._commandNotFound = commandNotFound;
	}
	
	/* (non-Javadoc)
	 * @see ist.meic.pa.command.exception.CommandException#getMessage()
	 */
	@Override
	public String getMessage() {
		return "Inserted command " + _commandNotFound + " not found.";
	}
	
}
