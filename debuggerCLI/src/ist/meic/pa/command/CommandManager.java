package ist.meic.pa.command;

public final class CommandManager {

	private final static Command[] COMMAND_LIST = { new AbortCommand(),
			new GetCommand(), new InfoCommand(), new RetryCommand(),
			new SetCommand(), new ThrowCommand() };

	public static Command executeCommand(Exception exception, String args) {

		String[] commandInput = args.split(" ");

		try {

			for (Command c : COMMAND_LIST) {
				if (commandInput.equals(c.getCommandName())) {
					c.execute(commandInput, exception, null);

					return c;
				}
			}

		} catch (CommandException e) {
			System.err.println(e.toString());
		}

		return null;

	}
}
