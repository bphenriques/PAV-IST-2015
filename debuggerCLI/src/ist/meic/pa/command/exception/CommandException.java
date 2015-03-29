package ist.meic.pa.command.exception;


/**
 * The Class CommandException.
 */
public class CommandException extends Exception {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -7148502222484812748L;
	
	/** The error message. */
	private String _errorMessage;
	
	/**
	 * Instantiates a new command exception.
	 */
	public CommandException() {
		super();
	}
	
	/**
	 * Instantiates a new command exception.
	 *
	 * @param errorMessage the error message
	 */
	public CommandException(String errorMessage) {
		this._errorMessage = errorMessage;
	}


	
	/* (non-Javadoc)
	 * @see java.lang.Throwable#getMessage()
	 */
	@Override
	public String getMessage() {
		return _errorMessage;
	}
	
}
