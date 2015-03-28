package ist.meic.pa.debugger.command.extension;

import ist.meic.pa.command.common.extension.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.DebuggerCLIStackManager;
import ist.meic.pa.debugger.MethodPrint;
import ist.meic.pa.debugger.command.ReturnableCommand;

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

		MethodPrint lastCalledMethod = DebuggerCLIStackManager
				.getMostRecentMethodCall();
		Class<?> returnType = lastCalledMethod.getReturnType();

		if (returnType == void.class) {
			if (args.length > 1) {
				throw new WrongNumberOfArgumentsException(0, args.length - 1);
			} else {
				_result = null;
				return;
			}
		}

		executeNonVoidReturn(args, returnType);
	}

	private void executeNonVoidReturn(String[] args, Class<?> returnType)
			throws CommandException {

		if (args.length > 2)
			throw new WrongNumberOfArgumentsException(1, args.length - 1);

		ObjectContructorFromString c = new ObjectContructorFromString(returnType, args[1]);
		Object returnObj = c.convert();
		_result = returnObj;
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}
}
