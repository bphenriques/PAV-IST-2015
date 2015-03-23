package ist.meic.pa.command;

public class NonReturnableCommandException extends CommandException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String _command;
	
	public NonReturnableCommandException(String commandName) {
		super();
		_command = commandName;
	}
	
	@Override
	public String getMessage(){
		return "Command \""+ _command + "\" doesn't have a return";
	}

}
