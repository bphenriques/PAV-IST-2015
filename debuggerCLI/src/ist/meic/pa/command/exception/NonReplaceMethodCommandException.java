package ist.meic.pa.command.exception;

public class NonReplaceMethodCommandException extends CommandException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String _command;
	
	public NonReplaceMethodCommandException(String commandName) {
		super();
		_command = commandName;
	}
	
	@Override
	public String getMessage(){
		return "Command \""+ _command + "\" doesn't replace the current method";
	}

}
