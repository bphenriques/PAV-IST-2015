package ist.meic.pa.debugger.command;

import ist.meic.pa.MethodPrint;
import ist.meic.pa.debugger.DInterface;

import java.lang.reflect.Field;
import java.util.Iterator;

public class InfoCommand extends Command {

	private static final String COMMAND_NAME = "Info";

	@Override
	public void execute(String[] args, Exception exception) {
		printCallStack(exception);
	}

	@Override
	public void execute(String[] args, Exception exception, Object target) {
		printObjectInfo(target);
		printCallStack(exception);
	}
	

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

	private void printCallStack(Exception exception) {
		
		System.out.println("Call stack:");
		Iterator<MethodPrint> iterator = DInterface.getStackIterator();
		
		while(iterator.hasNext()) {
			MethodPrint mp = iterator.next();
			System.out.print(mp.getMethodName());
			printArguments(mp);
		}
	}
	
	private void printArguments(MethodPrint mp) {
		System.out.print("(");
		Object[] args=mp.getArguments();
		for(int i=0;i<(args.length-1);i++) {
			System.out.print(args[i] + ", ");
		}
		System.out.println(args[args.length-1] + ")");
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
