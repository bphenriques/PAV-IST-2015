package ist.meic.pa.debugger;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.command.CommandManager;

import java.lang.reflect.Method;
import java.util.Scanner;

public final class DInterfaceSimple {

	private final static Scanner sc = new Scanner(System.in);

	public Object run(Class<?> targetClass, Object target,
			Class<?> returnType, String methodName, Class<?> parameterTypes[],
			Object args[]) throws Throwable {
		/*
		 * System.out.println("----------------------------------------");
		 * System.out.println("Class: " + className);
		 * System.out.println("target: " + target);
		 * System.out.println("returnType: " + returnType.getName());
		 * System.out.println("MethodName" + methodName);
		 * System.out.println("Parameter types " + parameterTypes);
		 * System.out.println("Args: " + args);
		 * System.out.println("----------------------------------------");
		 */

		DebuggerCLIStackManager.push(new MethodPrint(targetClass, methodName, args));
		
		
		Method callingMethod = targetClass.getDeclaredMethod(
				methodName, parameterTypes);
		boolean previousAccessibility = callingMethod.isAccessible();
		callingMethod.setAccessible(true);

		
		Object returnObject = null;
		
		boolean debug = true;
		while (debug) {
			try {
				returnObject = callingMethod.invoke(target, args);
				debug = false;
			} catch (Exception e) {
				Command command = debugMethod(e.getCause(), targetClass, target);
				if (command.isReturnable()) {
					returnObject = command.getResult();
					debug = false;
				}else if (command.isRetriable()) {
					continue;
				}	
			}
		}

		callingMethod.setAccessible(previousAccessibility);
		DebuggerCLIStackManager.pop();
		
		return returnObject;
	}

	private Command debugMethod(Throwable thrownException, Class<?> targetClass, Object target)
			throws Throwable {
		System.out.println(thrownException);
	
		CommandManager cm = new CommandManager();	
		while (true) {
			System.out.print("DebuggerCLI:> ");
			System.out.flush();

			String input = sc.nextLine();

			try {
				Command c = cm.executeCommand(thrownException, input, targetClass, target);
				
				if (c.isReturnable() || c.isRetriable()){
					return c;
				}
			} catch (CommandException e) {
				System.err.println("DEBUGGER ERROR : " + e);
			}
		}
	}

}
