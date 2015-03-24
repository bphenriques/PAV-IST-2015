package ist.meic.pa.command;

import java.lang.reflect.Field;

public class InfoCommand extends Command {

	private static final String NAME = "Info";

	@Override
	public void execute(String[] args, Exception exception) {
		// TODO Auto-generated method stub

	}

	@Override
	public void execute(String[] args, Exception exception, Object target) {
		printObjectInfo(target);
		printCallStack(exception);
	}
	

	@Override
	public String getCommandName() {
		return NAME;
	}

	private void printCallStack(Exception exception) {
		StackTraceElement[] stack = exception.getStackTrace();
		System.out.println("Call stack:");
		
		for (StackTraceElement stackElement : stack) {
			
		}
	}
	
	private void printObjectInfo(Object target) {
		System.out.println("Called Object:" + target);

		Class<?> targetClass = target.getClass();
		Field[] fields = targetClass.getDeclaredFields();

		if(fields.length>0) {
			System.out.println("       Fields:" + fields[0].getName());
		}
		
		for (int i = 1; i < fields.length; i++) {
			System.out.println("              " + fields[i].getName()); 
		}
	}

}
