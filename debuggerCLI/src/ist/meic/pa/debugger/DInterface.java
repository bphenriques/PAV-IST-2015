package ist.meic.pa.debugger;

import ist.meic.pa.MethodPrint;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.command.CommandManager;

import java.util.ConcurrentModificationException;
import java.util.Enumeration;
import java.util.Scanner;
import java.util.Stack;

public final class DInterface {

	private final static Stack<MethodPrint> _stack = new Stack<MethodPrint>();
	private final static Scanner sc = new Scanner(System.in);

	public static Command run(Exception thrownException) throws Exception {
		return run(thrownException, null);
	}
	
	public static Command run(Exception thrownException, Object target)
			throws Exception {

		String input;
		System.out.println(thrownException);

		while (true) {
			printCommandPrompt();
			input = sc.nextLine();

			try {
				Command c = CommandManager.executeCommand(thrownException, input, target);
				if (c.isReturnable() || c.isRetriable()) {
					return c;
				}

			} catch (CommandException e) {
				System.err.println(e.toString());
			} catch (ConcurrentModificationException e) {
				e.printStackTrace();
				System.err.println(e);
			}
		}

	}


	public static void pushToStack(String className, String methodName, Object[] args) {
		
		MethodPrint method = new MethodPrint(className, methodName, args);
		_stack.push(method);
	}

	public static void popStack() {
		_stack.pop();
	}
	
	public static MethodPrint getMostRecentMethodCall(){
		return _stack.get(_stack.size() - 1);
	}
	
	public static Enumeration<MethodPrint> getStackEnumeration() {
		
		
	return	_stack.elements();
		

	}

	private static void printCommandPrompt() {
		System.out.print("DebuggerCLI:> ");
		System.out.flush();
	}

}
