package ist.meic.pa;

import ist.meic.pa.command.Command;
import ist.meic.pa.command.CommandManager;

import java.util.Scanner;

public final class DInterface {

	private final static String STACK_TRACE_PRINT = "Inside %s.%s";

	


	public static Object run(Exception thrownException) throws Exception {
		return run(thrownException, null);
	}
	
	public static Object run(Exception thrownException, Object target) throws Exception {
		Scanner sc = new Scanner(System.in);
		try {
			String input;
			printClassesInStack(thrownException.getStackTrace());

			
			while (true) {
				printCommandPrompt();
				input = sc.next();
				
				Command c = CommandManager.executeCommand(thrownException, input);
				if(c.isReturnable()) {
					return c.getResult();
				}
			}
			
		} finally {
			sc.close();
		}

	}

	private static void printCommandPrompt() {
		System.out.print("DebuggerCLI:> ");
		System.out.flush();
	}

	private static void printClassesInStack(StackTraceElement[] stackTraceElements) {
		for (StackTraceElement ste : stackTraceElements) {
			String className = ste.getClassName();
			String methodName = ste.getMethodName();
			if(methodName.contains("$original")) continue;
			System.out.println(String.format(STACK_TRACE_PRINT, className,
					methodName));
			if (methodName.equals("main")) return;
		}

	}

}
