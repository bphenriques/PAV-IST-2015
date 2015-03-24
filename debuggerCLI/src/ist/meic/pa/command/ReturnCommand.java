package ist.meic.pa.command;

import ist.meic.pa.DInterface;
import ist.meic.pa.MethodPrint;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ReturnCommand extends ReturnableCommand {

	private static final String COMMAND_NAME = "Return";

	@Override
	public void execute(String[] args, Exception exception) {
		// TODO Auto-generated method stub

	}

	@Override
	public void execute(String[] args, Exception exception, Object target) throws WrongNumberOfArgumentsException {

		try {
			
			if(args.length != 2){
				throw new WrongNumberOfArgumentsException(1, args.length);
			}
			
			String returnString = args[1];

			Class<?> targetClass = target.getClass();
			MethodPrint methodPrint;

			Iterator<MethodPrint> it = DInterface.getStackIterator();

			methodPrint = it.next();
			String methodName = methodPrint.getMethodName();
			Method method = targetClass.getMethod(methodName);
			Class<?> returnClass = method.getReturnType();

			Constructor<?> constructor = returnClass.getConstructor(String.class);
			_result = constructor.newInstance(returnString);
			
			
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InstantiationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}
}
