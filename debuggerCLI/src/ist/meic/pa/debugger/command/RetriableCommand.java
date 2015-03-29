package ist.meic.pa.debugger.command;

/**
 * The abstract class Retriable represents commands that retries the
 * execution of the method
 */
public abstract class RetriableCommand extends Command {
	
	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#isRetriable()
	 */
	@Override
	public boolean isRetriable() {
		return true;
	}
}
