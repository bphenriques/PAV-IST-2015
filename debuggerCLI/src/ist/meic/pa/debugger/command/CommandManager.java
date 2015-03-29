package ist.meic.pa.debugger.command;

import ist.meic.pa.command.exception.CommandNotFoundException;

/**
 * The CommandManager class contains the executeCommand method, which finds the
 * right command to execute given the user input in string form.
 * 
 *
 */
public final class CommandManager {

	/** The supported commands. */
	private Command[] _supportedCommands;

	/**
	 * Instantiates a new command manager.
	 *
	 * @param supportedCommands the supported commands
	 */
	public CommandManager(Command[] supportedCommands) {
		this._supportedCommands = supportedCommands;
	}

	/**
	 * Execute command.
	 *
	 * @param exception the exception
	 * @param args the args
	 * @param targetClass the target class
	 * @param targetObj the target obj
	 * @return the command
	 * @throws Throwable the throwable
	 */
	public Command executeCommand(Throwable exception, String args,
			Class<?> targetClass, Object targetObj) throws Throwable {

		String[] commandInput = args.split(" ");

		for (Command c : _supportedCommands) {
			if (commandInput[0].equals(c.getCommandName())) {

				if (targetObj == null) {
					c.execute(commandInput, exception, targetClass);
				} else {
					c.execute(commandInput, exception, targetObj);
				}

				return c;
			}
		}

		throw new CommandNotFoundException(commandInput[0]);

	}
}
