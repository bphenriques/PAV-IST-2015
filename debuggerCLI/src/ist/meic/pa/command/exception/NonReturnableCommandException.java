package ist.meic.pa.command.exception;

/**
 * The Class NonReturnableCommandException should be thrown when the command
 * doesn't have a result to return and is requested to do so.
 */
public class NonReturnableCommandException extends CommandException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The _command. */
	private String _command;

	/**
	 * Instantiates a new non returnable command exception.
	 *
	 * @param commandName
	 *            the command name
	 */
	public NonReturnableCommandException(String commandName) {
		super();
		_command = commandName;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ist.meic.pa.command.exception.CommandException#getMessage()
	 */
	@Override
	public String getMessage() {
		return "Command \"" + _command + "\" doesn't have a return";
	}

}
