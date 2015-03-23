package ist.meic.pa.command;

public final class CommandManager {

	private final static Command[] COMMAND_LIST = { new AbortCommand(),
			new GetCommand(), new InfoCommand(), new RetryCommand(),
			new SetCommand(), new ThrowCommand() };

	public static Command executeCommand(Exception exception, String args) throws CommandException {

		String[] commandInput = args.split(" ");
		
		for(String a: commandInput) {
			System.out.println(a);
		}
		
		for (Command c : COMMAND_LIST) {
			if (commandInput[0].equals(c.getCommandName())) {
				c.execute(commandInput, exception, null);
				return c;
			}
		}

		throw new CommandNotFoundException(commandInput[0]);

	}
}
