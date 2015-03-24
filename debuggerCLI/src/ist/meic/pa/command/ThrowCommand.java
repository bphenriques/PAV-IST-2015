package ist.meic.pa.command;

public class ThrowCommand extends Command {

	private static final String COMMAND_NAME = "Throw";

	@Override
	public void execute(String[] args, Exception exception) throws Exception {
		throw exception;
	}
	
	
	@Override
	public void execute(String[] args, Exception exception, Object target) throws Exception {
		throw exception;
	}
	
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
