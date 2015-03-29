package ist.meic.pa.debugger.command.simple;

import ist.meic.pa.command.common.ClassUtil;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.stack.StackElement;
import ist.meic.pa.debugger.stack.StackManager;

import java.lang.reflect.Field;
import java.util.Enumeration;
import java.util.List;

/**
 * The InfoCommand class is a command for use in the debugger, representing the
 * user "Info" command.
 * <p>
 * Prints the call stack and information about the current object in the
 * following form:
 * <p>
 * Called Object: &lt;called object or null if static&gt;<br>
 * Fields: &lt;field1&gt; ... &lt;fieldN&gt;<br>
 * Call stack:<br>
 * &lt;called class&gt;.&lt;called method&gt;(&lt;arg1&gt;,...,&lt;argN&gt;)<br>
 * &lt;called class&gt;.&lt;called method&gt;(&lt;arg1&gt;,...,&lt;argN&gt;)<br>
 * ...
 */
public class InfoCommand extends Command {

	private static final String COMMAND_NAME = "Info";

	public static final ClassUtil fieldFinder = new ClassUtil();

	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {
		
		if (args.length != 1)
			throw new WrongNumberOfArgumentsException(0, args.length - 1);
		
		printObjectInfo(targetClass, null);
		printCallStack(exception);
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		
		if (args.length != 1)
			throw new WrongNumberOfArgumentsException(0, args.length - 1);
		
		printObjectInfo(target.getClass(), target);
		printCallStack(exception);
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

	private void printCallStack(Throwable exception) {

		System.out.println("Call stack:");
		Enumeration<StackElement> methodPrintEnumeration = StackManager
				.getStackEnumeration();

		StackElement methodPrint;

		String stackString;

		while (methodPrintEnumeration.hasMoreElements()) {
			methodPrint = methodPrintEnumeration.nextElement();
			stackString = methodPrint.getIvokingClass().getName();
			stackString += "." + methodPrint.getMethodName();

			Object[] argumentArray = methodPrint.getArguments();
			if (methodPrintEnumeration.hasMoreElements() == false) {
				// last element is main and it should be treated differently, it
				// should print its array
				// Warning: if Main isn't last method in stack it will fail
				Class<?> mainArgumentsClass = argumentArray[0].getClass();

				if (mainArgumentsClass.isArray()) {
					Object[] mainArguments = (Object[]) argumentArray[0];
					stackString += arrayToArgumentString(mainArguments);
				}

			} else {

				stackString += arrayToArgumentString(argumentArray);

			}

			System.out.println(stackString);
		}

	}

	private String arrayToArgumentString(Object[] argumentArray) {
		String result = "(";
		for (int i = 0; i < argumentArray.length; i++) {
			result += argumentArray[i];

			if (i != argumentArray.length - 1)
				result += ",";
		}

		result += ")";

		return result;
	}

	private void printObjectInfo(Class<?> targetClass, Object target) {

		String calledObjectString = "Called Object:\t";

		if (target == null) {
			calledObjectString += target;
		} else {
			calledObjectString += target.getClass().getName()
					+ "@" + Integer.toHexString(target.hashCode());
		}
		System.out.println(calledObjectString);
		
		
		List<Field> fields = ClassUtil.getDeclaredFields(targetClass);

		System.out.print("       Fields:\t");
		for (Field f : fields) {
			System.out.print(f.getName() + " ");
		}

		System.out.println();
	}

}
