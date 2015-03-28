package ist.meic.pa.debugger.command;

/**
 * The abstract class ReturnableCommand represents commands that returns a value
 * from the exception causing method.
 *
 */
public abstract class ReturnableCommand extends Command {

	protected Object _result;

	@Override
	public boolean isReturnable() {
		return true;
	}

	@Override
	public Object getResult() {
		return _result;
	}
}
