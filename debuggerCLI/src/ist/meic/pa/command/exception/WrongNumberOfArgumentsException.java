package ist.meic.pa.command.exception;

/**
 * The Class WrongNumberOfArgumentsException should be thrown when the user
 * fails to provide the correct number of arguments to a command.
 */
public class WrongNumberOfArgumentsException extends CommandException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The number of arguments expected. */
	private int _expected;

	/** The number of arguments provided by the user. */
	private int _gotten;

	/**
	 * Instantiates a new wrong number of arguments exception.
	 *
	 * @param expected
	 *            the expected
	 * @param gotten
	 *            the gotten
	 */
	public WrongNumberOfArgumentsException(int expected, int gotten) {
		super();
		_expected = expected;
		_gotten = gotten;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ist.meic.pa.command.exception.CommandException#getMessage()
	 */
	@Override
	public String getMessage() {
		return "Error while executing command: Expected " + _expected
				+ " arguments but was provided " + _gotten;
	}

}
