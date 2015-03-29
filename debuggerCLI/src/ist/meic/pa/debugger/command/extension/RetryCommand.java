package ist.meic.pa.debugger.command.extension;

import ist.meic.pa.command.common.extension.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.RetriableCommand;
import ist.meic.pa.debugger.stack.StackElement;
import ist.meic.pa.debugger.stack.StackManager;

/**
 * The RetryCommand class is a command for use in the debugger, representing the
 * user "Retry <arg1> <arg2> ..." command. 
 * <p>
 * Retrys the execution of the method with different parameters
 */
public class RetryCommand extends RetriableCommand {

	/** The Constant COMMAND_NAME. */
	private static final String COMMAND_NAME = "Retry";

	/** The _result. */
	private Object[] _result = null;
	
	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[], java.lang.Throwable, java.lang.Class)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {
		
		
		if (args.length == 1){
			return;
		}
		
		executeRetryCommand(args);
	}

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[], java.lang.Throwable, java.lang.Object)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		
		if (args.length == 1){
			return;
		}
	
		executeRetryCommand(args);
	}

	/**
	 * Execute retry command.
	 *
	 * @param args the args
	 * @throws CommandException the command exception
	 */
	private void executeRetryCommand(String[] args) throws CommandException {
		StackElement lastCalledMethod = StackManager.getMostRecentMethodCall();
		
		Class<?>[] parameterTypes = lastCalledMethod.getParameterTypes();
		
		if(parameterTypes.length != (args.length - 1)){
			throw new WrongNumberOfArgumentsException(parameterTypes.length, args.length - 1);
		}
		
		_result = new Object[parameterTypes.length];	
		for(int i = 1; i < args.length; i++){
			_result[i-1] = new ObjectContructorFromString(parameterTypes[i-1], args[i]).convert();
		}
				
	}
	
	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#isReplaceArguments()
	 */
	@Override
	public boolean isReplaceArguments() {
		return true;
	}
	
	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#getArgumentsResult()
	 */
	@Override
	public Object[] getArgumentsResult(){
		
		if(_result != null){
			Object[] copy = new Object[_result.length];
			for(int i = 0; i < copy.length; i++){
				copy[i] = _result[i];
			}
			
			//avoids bugs caused by executing Retry <args> followed by Retry with no args
			_result = null;
			
			return copy;
		}
		
		return null;
	}

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#getCommandName()
	 */
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}
}
