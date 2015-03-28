package ist.meic.pa.command.exception;

/**
 * The Class NonReplaceMethodCommandException should be thrown when the command
 * can't give a method back as a result and is requested to do so.
 */
public class NonReplaceMethodCommandException extends CommandException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The command. */
	private String _command;

	/**
	 * Instantiates a new non replace method command exception.
	 *
	 * @param commandName
	 *            the command name
	 */
	public NonReplaceMethodCommandException(String commandName) {
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
		return "Command \"" + _command
				+ "\" doesn't replace the current method";
	}

}
