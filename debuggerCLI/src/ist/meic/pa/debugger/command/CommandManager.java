package ist.meic.pa.debugger.command;

import ist.meic.pa.command.exception.CommandNotFoundException;

public final class CommandManager {

	private final static Command[] COMMAND_LIST = { new AbortCommand(),
			new GetCommand(), new InfoCommand(), new RetryCommand(),
			new SetCommand(), new ThrowCommand(), new ReturnCommand() };

	public static Command executeCommand(Throwable exception, String args,
			Object target) throws Throwable {

		String[] commandInput = args.split(" ");

		for (Command c : COMMAND_LIST) {
			if (commandInput[0].equals(c.getCommandName())) {
				c.execute(commandInput, exception, target);
				return c;
			}
		}

		throw new CommandNotFoundException(commandInput[0]);

	}
}
