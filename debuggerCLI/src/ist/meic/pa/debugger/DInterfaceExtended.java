package ist.meic.pa.debugger;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.debugger.command.AbortCommand;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.command.CommandManager;
import ist.meic.pa.debugger.command.GetCommand;
import ist.meic.pa.debugger.command.InfoCommand;
import ist.meic.pa.debugger.command.ReplaceCommand;
import ist.meic.pa.debugger.command.RetryCommand;
import ist.meic.pa.debugger.command.ReturnCommand;
import ist.meic.pa.debugger.command.SetCommand;
import ist.meic.pa.debugger.command.ThrowCommand;

import java.lang.reflect.Method;
import java.util.Scanner;

public final class DInterfaceExtended extends DInterface {

	private final static Scanner sc = new Scanner(System.in);
	private final static CommandManager commandsManager = new CommandManager(new Command[]{
		new AbortCommand(),
		new GetCommand(), 
		new InfoCommand(), 
		new RetryCommand(),
		new SetCommand(), 
		new ThrowCommand(), 
		new ReturnCommand(),
		new ReplaceCommand()
	});
	
	@Override
	protected Object invokeMethodWithDebug(Class<?> targetClass, Object target, Method callingMethod, Object args[]) throws Throwable{
		Object returnObject = null;
		boolean debug = true;
		while (debug) {
			try {
				System.out.println("Calling " + callingMethod.getName());
				returnObject = callingMethod.invoke(target, args);
				debug = false;
			} catch (Exception e) {
				Command command = debugMethod(e.getCause(), targetClass, target);
				if (command.isReturnable()) {
					System.out.println("1");
					returnObject = command.getResult();
					debug = false;
				} else if (command.isRetriable()) {
					System.out.println("2");
					continue;
				} else if(command.isReplaceMethod()){
					System.out.println("3");
					callingMethod = command.getMethodResult();
					System.out.println("REPLACED METHOD WITH: " + command.getMethodResult().getName());
					continue;
				}
				
				System.out.println("4");
				
			}
		}
		
		return returnObject;
	}


	
	private Command debugMethod(Throwable thrownException,
			Class<?> targetClass, Object target) throws Throwable {
		System.out.println(thrownException);

		while (true) {
			System.out.print("DebuggerCLI:> ");
			System.out.flush();

			String input = sc.nextLine();

			try {
				Command c = commandsManager.executeCommand(thrownException, input,
						targetClass, target);

				if (c.isReturnable() || c.isRetriable()) {
					return c;
				}
			} catch (CommandException e) {
				System.err.println("DEBUGGER ERROR : " + e);
			}
		}
	}

}
