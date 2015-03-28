package ist.meic.pa.debugger.command;

import ist.meic.pa.command.common.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.NonPrimitiveReturnException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.DebuggerCLIStackManager;
import ist.meic.pa.debugger.MethodPrint;

public class ReturnCommand extends ReturnableCommand {

	private static final String COMMAND_NAME = "Return";


	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		execute(args, exception, target.getClass());
	}
	
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {

		MethodPrint lastCalledMethod = DebuggerCLIStackManager.getMostRecentMethodCall();
		Class<?> returnType = lastCalledMethod.getReturnType();

		if (returnType == void.class){
			if(args.length > 1){
				throw new WrongNumberOfArgumentsException(0, args.length - 1);
			}else{
				_result = null;
				return;
			}
		}
		
		executeNonVoidReturn(args, returnType);
	}

	private void executeNonVoidReturn(String[] args, Class<?> returnType)
			throws NonPrimitiveReturnException, WrongNumberOfArgumentsException {
		
		if(!returnType.isPrimitive())
			throw new NonPrimitiveReturnException();
		
		if(args.length > 2)
			throw new WrongNumberOfArgumentsException(1, args.length - 1);
		
		ObjectContructorFromString c = new ObjectContructorFromString();
		Object returnObj = c.convert(returnType, args[1]);
		_result = returnObj;
	}

	
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}
}
