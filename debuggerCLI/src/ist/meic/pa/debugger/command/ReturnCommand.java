package ist.meic.pa.debugger.command;

import ist.meic.pa.command.common.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.NonPrimitiveReturnException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.DebuggerCLIStackManager;
import ist.meic.pa.debugger.MethodPrint;

import java.lang.reflect.Method;

import ist.meic.pa.command.common.FieldFinder;

public class ReturnCommand extends ReturnableCommand {

	private static final String COMMAND_NAME = "Return";

	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {

		MethodPrint lastCalledMethod = DebuggerCLIStackManager
				.getMostRecentMethodCall();
		String methodName = lastCalledMethod.getMethodName();

				
		// Warning: If it doesn't find the method it will result in a
		// nullpointer exception
		Method method = null;
		for (Method m : FieldFinder.getDeclaredMethods(targetClass)){
			if (m.getName().equals(methodName)) {
				method = m;
				break;
			}

		}
		boolean originalAccessibleValue = method.isAccessible();
		
		method.setAccessible(true);
		Class<?> returnType = method.getReturnType();
		method.setAccessible(originalAccessibleValue);
		

		if (returnType == void.class) {
			if(args.length == 1)
				_result = null;
			else
				throw new WrongNumberOfArgumentsException(0, args.length - 1);

		} else {
			
			if(!returnType.isPrimitive())
				throw new NonPrimitiveReturnException();
			if(args.length == 2){
				ObjectContructorFromString c = new ObjectContructorFromString();
				Object returnObj = c.convert(returnType, args[1]);
				_result = returnObj;
			}else{
				throw new WrongNumberOfArgumentsException(1, args.length - 1);
			}
				
				String returnValueString = args[1];
			executeReturn(targetClass, returnValueString);
			
		}
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {

		
		execute(args, exception, target.getClass());
		
		//FIXME:Nao sei se faz sentido
		/*try {

			/*
			 * if(args.length != 2){ throw new
			 * WrongNumberOfArgumentsException(1, args.length); }
			 *

			String returnValueString = args[1];
			Class<?> targetClass = target.getClass();

			executeReturn(targetClass, returnValueString);

		} catch (IllegalArgumentException | SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}*/
	}

	private void executeReturn(Class<?> targetClass, String returnValueString) throws CommandException {
		MethodPrint lastCalledMethod = DebuggerCLIStackManager
				.getMostRecentMethodCall();
		String methodPrefix = targetClass.getName() + ".";
		String methodFullName = lastCalledMethod.getMethodName();
		String methodName = methodFullName.replace(methodPrefix, "");

		Method method = null;
		/* overloading methods always returns the same type */
		for (Method m : targetClass.getDeclaredMethods()) {
			if (m.getName().equals(methodName)) {
				method = m;
				break;
			}

		}

		boolean originalAccessibleValue = method.isAccessible();

		method.setAccessible(true);

		Class<?> returnClass = method.getReturnType();
		ObjectContructorFromString c = new ObjectContructorFromString();
		Object returnObj = c.convert(returnClass, returnValueString);

		method.setAccessible(originalAccessibleValue);

		_result = returnObj;
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}
}
