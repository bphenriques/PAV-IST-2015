package ist.meic.pa.debugger;


import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.command.CommandManager;
import ist.meic.pa.debugger.command.extension.ReplaceCommand;
import ist.meic.pa.debugger.command.extension.ReturnCommand;
import ist.meic.pa.debugger.command.extension.SetCommand;
import ist.meic.pa.debugger.command.simple.AbortCommand;
import ist.meic.pa.debugger.command.simple.GetCommand;
import ist.meic.pa.debugger.command.simple.InfoCommand;
import ist.meic.pa.debugger.command.simple.RetryCommand;
import ist.meic.pa.debugger.command.simple.ThrowCommand;

import java.lang.reflect.Method;
import java.util.Scanner;

/**
 * The Class DInterfaceExtended.
 */
public final class DInterfaceExtended extends DInterface {

	/** The Constant sc. */
	private final static Scanner sc = new Scanner(System.in);
	
	/** The Constant commandsManager. */
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
	
	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.DInterface#invokeMethodWithDebug(java.lang.Class, java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	@Override
	protected Object invokeMethodWithDebug(Class<?> targetClass, Object target, Method callingMethod, Object args[]) throws Throwable{
		while (true) {
			try {
				return callingMethod.invoke(target, args);
			} catch (Exception e) {
				Command command = debugMethod(e.getCause(), targetClass, target);
				if (command.isReturnable()) {
					return command.getResult();
				}else if(command.isReplaceMethod()){
					callingMethod = command.getMethodResult();
				}
			}
		}
	}
	
	/**
	 * Debug method.
	 *
	 * @param thrownException the thrown exception by the user's code
	 * @param targetClass the target class
	 * @param target the target instance of the class
	 * @return the command instance
	 * @throws Throwable the throwable
	 */
	private Command debugMethod(Throwable thrownException,
			Class<?> targetClass, Object target) throws Throwable {
		System.out.println(thrownException);

		while (true) {
			System.out.print("DebuggerCLI:> ");
			System.out.flush();
			String input = sc.nextLine();
			
			try {
				Command c = commandsManager.executeCommand(thrownException, input, targetClass, target);
				if (c.shouldExitDebugger()) {
					return c;
				}
				
			} catch (CommandException e) {
				System.err.println("DEBUGGER ERROR : " + e);
			}
		}
	}

}