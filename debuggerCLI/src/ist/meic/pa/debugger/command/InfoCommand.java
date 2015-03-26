package ist.meic.pa.debugger.command;

import ist.meic.pa.MethodPrint;
import ist.meic.pa.debugger.DInterface;

import java.lang.reflect.Field;
import java.util.Enumeration;

public class InfoCommand extends Command {

	private static final String COMMAND_NAME = "Info";

	@Override
	public void execute(String[] args, Throwable exception) {
		printCallStack(exception);
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target) {
		printObjectInfo(target);
		printCallStack(exception);
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

	private void printCallStack(Throwable exception) {

		System.out.println("Call stack:");
		Enumeration<MethodPrint> methodPrintEnumeration = DInterface.getStackEnumeration();

		MethodPrint methodPrint;

		String stackString;
		
		while (methodPrintEnumeration.hasMoreElements()) {
			
			methodPrint = methodPrintEnumeration.nextElement();
			
			
			stackString = methodPrint.getMethodName() + "(";
			
			int i;
			Object[] argumentArray = methodPrint.getArguments();
			for (i = 0;  i < argumentArray.length; i++){
				stackString += argumentArray[i].toString();
				
				if(i != argumentArray.length - 1)
					stackString += ",";
				else
					stackString += ")";
			}
			
			System.out.println(stackString);
			
		}

	}


	private void printObjectInfo(Object target) {
		System.out.println("Called Object:" + target);

		Class<?> targetClass = target.getClass();
		Field[] fields = targetClass.getDeclaredFields();

		if (fields.length > 0) {
			System.out.println("       Fields:" + fields[0].getName());
		}

		for (int i = 1; i < fields.length; i++) {
			System.out.println("              " + fields[i].getName());
		}
	}

}
