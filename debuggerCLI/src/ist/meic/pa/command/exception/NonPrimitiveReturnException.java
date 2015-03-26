package ist.meic.pa.command.exception;


public class NonPrimitiveReturnException extends CommandException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public String getMessage() {
		return "Function that don't have primitive return types can't be returned.";
	}
	

}
