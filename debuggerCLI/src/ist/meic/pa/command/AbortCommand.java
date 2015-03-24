package ist.meic.pa.command;



public class AbortCommand extends Command {

	private final static String _name = "Abort";
	
	
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
		return _name;
	}

}
