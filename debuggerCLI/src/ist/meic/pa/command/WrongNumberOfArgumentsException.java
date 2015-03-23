package ist.meic.pa.command;

public class WrongNumberOfArgumentsException extends CommandException {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private int _expected;
	private int _gotten;

	public WrongNumberOfArgumentsException(int expected, int gotten) {
		super();
		_expected = expected;
		_gotten = gotten;
	}
	
	@Override
	public String toString(){
		return "Error while executing command: Expected "+ _expected + " arguments but was provided " + _gotten;
	}
	

}
