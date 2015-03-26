package ist.meic.pa.debugger.command;

import ist.meic.pa.command.exception.CommandNotFoundException;

public final class CommandManager {
	
	private final static Command[] COMMAND_LIST = { new AbortCommand(),
			new GetCommand(), new InfoCommand(), new RetryCommand(),
			new SetCommand(), new ThrowCommand(), new ReturnCommand() };

	public Command executeCommand(Throwable exception, String args,
			Class<?> targetClass, Object targetObj) throws Throwable {

		String[] commandInput = args.split(" ");

		for (Command c : COMMAND_LIST) {
			if (commandInput[0].equals(c.getCommandName())) {
				
				if(targetObj == null){
					System.out.println("CALLING STATIC METHOD");
					c.execute(commandInput, exception, targetClass);
				}else{
					System.out.println("CALLING OBJECT METHOD");
					c.execute(commandInput, exception, targetObj);
				}
				
				return c;
			}
		}

		throw new CommandNotFoundException(commandInput[0]);

	}
}
