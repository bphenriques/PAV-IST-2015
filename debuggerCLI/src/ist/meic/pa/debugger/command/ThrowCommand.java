package ist.meic.pa.debugger.command;

public class ThrowCommand extends Command {

	private static final String COMMAND_NAME = "Throw";

	@Override
	public void execute(String[] args, Throwable exception) throws Throwable {
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
