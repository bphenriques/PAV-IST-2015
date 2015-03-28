package ist.meic.pa.debugger.command;

import ist.meic.pa.command.common.FieldFinder;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.debugger.DebuggerCLIStackManager;
import ist.meic.pa.debugger.MethodPrint;

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

	public static final FieldFinder fieldFinder = new FieldFinder();

	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {
		printObjectInfo(targetClass, null);
		printCallStack(exception);
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {
		printObjectInfo(target.getClass(), target);
		printCallStack(exception);
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

	private void printCallStack(Throwable exception) {

		System.out.println("Call stack:");
		Enumeration<MethodPrint> methodPrintEnumeration = DebuggerCLIStackManager
				.getStackEnumeration();

		MethodPrint methodPrint;

		String stackString;

		while (methodPrintEnumeration.hasMoreElements()) {
			methodPrint = methodPrintEnumeration.nextElement();
			stackString = methodPrint.getIvokingClass().getName();
			stackString += "." + methodPrint.getMethodName() + "(";

			int i;
			Object[] argumentArray = methodPrint.getArguments();
			for (i = 0; i < argumentArray.length; i++) {
				stackString += argumentArray[i];

				if (i != argumentArray.length - 1)
					stackString += ",";
			}

			stackString += ")";
			System.out.println(stackString);

		}

	}

	private void printObjectInfo(Class<?> targetClass, Object target) {
		System.out.println("Called Object:\t" + target.getClass().getName()
				+ "@" + Integer.toHexString(target.hashCode()));

		List<Field> fields = FieldFinder.getDeclaredFields(targetClass);

		System.out.print("       Fields:\t");
		for (Field f : fields) {
			System.out.print(f.getName() + " ");
		}

		System.out.println();
	}

}
