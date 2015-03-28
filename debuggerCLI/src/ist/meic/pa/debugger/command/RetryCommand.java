package ist.meic.pa.debugger.command;

import ist.meic.pa.command.exception.CommandException;

/**
 * The RetryCommand class is a command for use in the debugger, representing the
 * user "Retry" command.
 * <p>
 * Runs the exception causing method again.
 */
public class RetryCommand extends Command {

	private static final String COMMAND_NAME = "Retry";

	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {
		/* intentionally left empty */
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		/* intentionally left empty */
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

	@Override
	public boolean isRetriable() {
		return true;
	}
}
