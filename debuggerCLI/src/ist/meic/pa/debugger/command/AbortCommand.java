package ist.meic.pa.debugger.command;

import ist.meic.pa.command.exception.CommandException;

public class AbortCommand extends Command {

	private final static String COMMAND_NAME = "Abort";
	
	
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass) throws CommandException, Throwable {
		System.exit(1);
	}
	
	@Override
	public void execute(String[] args, Throwable exception, Object target) throws CommandException, Throwable {
		System.exit(1);
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
