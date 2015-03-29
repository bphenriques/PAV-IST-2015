package ist.meic.pa.debugger.command.simple;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.RetriableCommand;

/**
 * The RetryCommand class is a command for use in the debugger, representing the
 * user "Retry" command.
 * <p>
 * Runs the exception causing method again.
 */
public class RetryCommand extends RetriableCommand {

	/** The Constant COMMAND_NAME. */
	private static final String COMMAND_NAME = "Retry";

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[], java.lang.Throwable, java.lang.Class)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {
		
		if (args.length != 1)
			throw new WrongNumberOfArgumentsException(0, args.length - 1);
		
		/* intentionally left empty */
	}

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[], java.lang.Throwable, java.lang.Object)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		
		if (args.length != 1)
			throw new WrongNumberOfArgumentsException(0, args.length - 1);
	}

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#getCommandName()
	 */
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}
}
