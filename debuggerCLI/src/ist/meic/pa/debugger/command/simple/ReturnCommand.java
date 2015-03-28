package ist.meic.pa.debugger.command.simple;

import ist.meic.pa.command.common.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.NonPrimitiveReturnException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.ReturnableCommand;
import ist.meic.pa.debugger.stack.StackElement;
import ist.meic.pa.debugger.stack.StackManager;

/**
 * The Class ReturnCommand provides the possibility of returning a function with
 * a given value.
 */
public class ReturnCommand extends ReturnableCommand {

	/** The Constant COMMAND_NAME. */
	private static final String COMMAND_NAME = "Return";

	/*
	 * (non-Javadoc)
	 * 
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[],
	 * java.lang.Throwable, java.lang.Object)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		execute(args, exception, target.getClass());
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

		StackElement lastCalledMethod = StackManager.getMostRecentMethodCall();
		Class<?> returnType = lastCalledMethod.getReturnType();

		if (returnType == void.class) {
			if (args.length != 1) {
				throw new WrongNumberOfArgumentsException(0, args.length - 1);
			} else {
				_result = null;
				return;
			}
		}

		executeNonVoidReturn(args, returnType);
	}

	/**
	 * Execute non void return.
	 *
	 * @param args
	 *            the args
	 * @param returnType
	 *            the return type
	 * @throws NonPrimitiveReturnException
	 *             the non primitive return exception
	 * @throws WrongNumberOfArgumentsException
	 *             the wrong number of arguments exception
	 */
	private void executeNonVoidReturn(String[] args, Class<?> returnType)
			throws NonPrimitiveReturnException, WrongNumberOfArgumentsException {

		if (!returnType.isPrimitive())
			throw new NonPrimitiveReturnException();

		if (args.length != 2)
			throw new WrongNumberOfArgumentsException(1, args.length - 1);

		ObjectContructorFromString c = new ObjectContructorFromString();
		Object returnObj = c.convert(returnType, args[1]);
		_result = returnObj;
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
