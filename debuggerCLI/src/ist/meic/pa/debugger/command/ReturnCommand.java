package ist.meic.pa.debugger.command;

import ist.meic.pa.MethodPrint;
import ist.meic.pa.command.common.ObjectContructorFromString;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.DInterface;

import java.lang.reflect.Method;

public class ReturnCommand extends ReturnableCommand {

	private static final String COMMAND_NAME = "Return";

	@Override
	public void execute(String[] args, Exception exception) throws WrongNumberOfArgumentsException {

		if(args.length != 2){
			throw new WrongNumberOfArgumentsException(1, args.length);
		}
		
		String returnValueString = args[1];	
		
		MethodPrint lastCalledMethod = DInterface.getMostRecentMethodCall();
		String className = lastCalledMethod.getClassName();
		
		try {
			Class<?> targetClass = Class.forName(className);
			executeReturn(targetClass, returnValueString);
		
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
		
	@Override
	public void execute(String[] args, Exception exception, Object target) throws WrongNumberOfArgumentsException {

		try {
			
			if(args.length != 2){
				throw new WrongNumberOfArgumentsException(1, args.length);
			}
			
			String returnValueString = args[1];	
			Class<?> targetClass = target.getClass();
			
			executeReturn(targetClass, returnValueString);
			
		} catch (IllegalArgumentException | SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private void executeReturn(Class<?> targetClass, String returnValueString) {
		MethodPrint lastCalledMethod = DInterface.getMostRecentMethodCall(); 
		String methodPrefix = targetClass.getName() + ".";
		String methodFullName = lastCalledMethod.getMethodName();
		String methodName = methodFullName.replace(methodPrefix, "");
		
		
		Method method = null;
		/* overloading methods always returns the same type */
		for (Method m : targetClass.getDeclaredMethods()){	
			if(m.getName().equals(methodName)){
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
