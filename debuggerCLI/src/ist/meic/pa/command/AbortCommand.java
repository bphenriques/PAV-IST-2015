package ist.meic.pa.command;



public class AbortCommand extends Command {

	private final static String COMMAND_NAME = "Abort";
	
	
	@Override
	public void execute(String[] args, Exception exception) {
		System.exit(0);
	}
	
	@Override
	public void execute(String[] args, Exception exception, Object target) {
		System.exit(0);
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
