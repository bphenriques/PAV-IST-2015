package ist.meic.pa.command.exception;



/**
 * The Class NonPrimitiveReturnException.
 */
public class NonPrimitiveReturnException extends CommandException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/* (non-Javadoc)
	 * @see ist.meic.pa.command.exception.CommandException#getMessage()
	 */
	@Override
	public String getMessage() {
		return "Function that don't have primitive return types can't be returned.";
	}
	

}
