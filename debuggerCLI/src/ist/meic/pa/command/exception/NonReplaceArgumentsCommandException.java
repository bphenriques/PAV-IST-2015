package ist.meic.pa.command.exception;

/**
 * The Class NonReplaceArgumentsCommandException should be thrown when the command
 * can't give the arguments back as a result and is requested to do so.
 */
public class NonReplaceArgumentsCommandException extends CommandException {

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
	public NonReplaceArgumentsCommandException(String commandName) {
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
				+ "\" doesn't replace the method call arguments";
	}

}
