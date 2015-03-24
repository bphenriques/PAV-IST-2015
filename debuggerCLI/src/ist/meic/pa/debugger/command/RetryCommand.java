package ist.meic.pa.debugger.command;

public class RetryCommand extends Command {

	private static final String COMMAND_NAME = "Retry";

	
	@Override
	public void execute(String[] args, Exception exception) {
		/* intentionally left empty */
	}
	
	@Override
	public void execute(String[] args, Exception exception, Object target) {
		/* intentionally left empty */
	}
	
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}
	
	@Override
	public boolean isRetriable(){
		return true;
	}

}
