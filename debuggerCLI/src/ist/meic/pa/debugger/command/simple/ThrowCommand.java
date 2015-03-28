package ist.meic.pa.debugger.command.simple;

import ist.meic.pa.debugger.command.Command;

/**
 * The ThrowCommand class is a command for use in the debugger, representing the
 * user "Throw" command.
 * <p>
 * Throws the caught exception to be handled by the next handler.
 */
public class ThrowCommand extends Command {

	private static final String COMMAND_NAME = "Throw";

	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass) throws Throwable {
		throw exception;
	}
	
	
	@Override
	public void execute(String[] args, Throwable exception, Object target) throws Throwable {
		throw exception;
	}
	
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}