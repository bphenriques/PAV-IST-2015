package ist.meic.pa.debugger.command.simple;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.Command;

/**
 * The AbortCommand class is a command for use in the debugger, representing the
 * user "Abort" command.
 * <p>
 * Aborts the program by calling System.exit
 *
 */
public class AbortCommand extends Command {

	private final static String COMMAND_NAME = "Abort";

	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {
		
		if (args.length != 1)
			throw new WrongNumberOfArgumentsException(0, args.length - 1);
		
		System.exit(1);
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		
		if (args.length != 1)
			throw new WrongNumberOfArgumentsException(0, args.length - 1);
		
		System.exit(1);
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
