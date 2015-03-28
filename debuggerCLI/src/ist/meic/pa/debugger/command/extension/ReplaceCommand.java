package ist.meic.pa.debugger.command.extension;

import ist.meic.pa.command.common.ClassUtil;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.stack.StackElement;
import ist.meic.pa.debugger.stack.StackManager;

import java.lang.reflect.Method;

public class ReplaceCommand extends Command {

	private static final String COMMAND_NAME = "Replace";
	
	private Method _methodResult;
	
	@Override
	public Method getMethodResult() {
		return _methodResult;
	}

	@Override
	public boolean isReplaceMethod(){
		return true;
	}
	
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {
		
		if (args.length != 2){
			throw new WrongNumberOfArgumentsException(2, args.length);
		}
		
		String methodName = args[1];
		
		_methodResult = executeAux(targetClass, methodName);	
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		
		if (args.length != 2){
			throw new WrongNumberOfArgumentsException(2, args.length);
		}

		String methodName = args[1];
		
		_methodResult = executeAux(target.getClass(), methodName);
	}

	private Method executeAux(Class<?> targetClass, String methodName)
			throws NoSuchMethodException {
		StackElement lastCalledMethod = StackManager.getMostRecentMethodCall();
		
		
		Class<?> params[] = lastCalledMethod.getParameterTypes();
		
		return ClassUtil.getDeclaredMethod(targetClass, methodName, params);
	}
	
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
