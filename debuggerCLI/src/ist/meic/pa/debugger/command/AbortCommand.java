package ist.meic.pa.debugger.command;



public class AbortCommand extends Command {

	private final static String COMMAND_NAME = "Abort";
	
	
	@Override
	public void execute(String[] args, Throwable exception) {
		System.exit(0);
	}
	
	@Override
	public void execute(String[] args, Throwable exception, Object target) {
		System.exit(0);
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
