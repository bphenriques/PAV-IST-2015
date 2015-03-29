package ist.meic.pa.command.exception;



/**
 * The Class NonPrimitiveReturnException.
 */
public class NonPrimitiveSetException extends CommandException {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/* (non-Javadoc)
	 * @see ist.meic.pa.command.exception.CommandException#getMessage()
	 */
	@Override
	public String getMessage() {
		return "Fields that are not of primitive types can not be set.";
	}
	

}
