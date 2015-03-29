package ist.meic.pa.debugger.command.extension;

import ist.meic.pa.command.common.extension.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.stack.StackElement;
import ist.meic.pa.debugger.stack.StackManager;

/**
 * The RetryCommand class is a command for use in the debugger, representing the
 * user "Retry" command.
 * <p>
 * Runs the exception causing method again.
 */
public class RetryCommand extends Command {

	private static final String COMMAND_NAME = "Retry";

	private Object[] _result = null;
	
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {
		
		if (args.length == 1){
			return;
		}
		
		System.out.println("....");
		executeRetryCommand(args);
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		
		if (args.length == 1){
			return;
		}
	
		executeRetryCommand(args);
	}

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
	
	@Override
	public boolean isReplaceArguments() {
		return true;
	}
	
	@Override
	public Object[] getArgumentsResult(){
				
		Object[] copy = new Object[_result.length];
		for(int i = 0; i < copy.length; i++){
			System.out.println(_result[i].getClass().getName());
			copy[i] = _result[i];
		}
		
		_result = null;
		
		return copy;
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

	@Override
	public boolean isRetriable() {
		return true;
	}
}
