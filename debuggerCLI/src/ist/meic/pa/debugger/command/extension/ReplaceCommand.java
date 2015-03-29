package ist.meic.pa.debugger.command.extension;

import ist.meic.pa.command.common.ClassUtil;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.stack.StackElement;
import ist.meic.pa.debugger.stack.StackManager;

import java.lang.reflect.Method;

/**
 * The Class ReplaceCommand call a different method that uses the same arguments
 * and returns the same type as the function where the exception was thrown.
 */
public class ReplaceCommand extends Command {

	/** The Constant COMMAND_NAME. */
	private static final String COMMAND_NAME = "Replace";

	/** The _method result. */
	private Method _methodResult;

	/*
	 * (non-Javadoc)
	 * 
	 * @see ist.meic.pa.debugger.command.Command#getMethodResult()
	 */
	@Override
	public Method getMethodResult() {
		return _methodResult;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ist.meic.pa.debugger.command.Command#isReplaceMethod()
	 */
	@Override
	public boolean isReplaceMethod() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[],
	 * java.lang.Throwable, java.lang.Class)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {

		if (args.length != 2) {
			throw new WrongNumberOfArgumentsException(1, args.length - 1);
		}

		String methodName = args[1];

		_methodResult = executeAux(targetClass, methodName);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[],
	 * java.lang.Throwable, java.lang.Object)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {

		if (args.length != 2) {
			throw new WrongNumberOfArgumentsException(1, args.length - 1);
		}

		String methodName = args[1];

		_methodResult = executeAux(target.getClass(), methodName);
	}

	/**
	 * Execute aux.
	 *
	 * @param targetClass
	 *            the target class
	 * @param methodName
	 *            the method name
	 * @return the method
	 * @throws NoSuchMethodException
	 *             the no such method exception
	 */
	private Method executeAux(Class<?> targetClass, String methodName)
			throws NoSuchMethodException {
		StackElement lastCalledMethod = StackManager.getMostRecentMethodCall();

		Class<?> params[] = lastCalledMethod.getParameterTypes();

		return ClassUtil.getDeclaredMethod(targetClass, methodName, params);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see ist.meic.pa.debugger.command.Command#getCommandName()
	 */
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
