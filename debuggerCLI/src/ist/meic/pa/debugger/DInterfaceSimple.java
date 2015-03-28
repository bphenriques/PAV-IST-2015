package ist.meic.pa.debugger;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.command.CommandManager;
import ist.meic.pa.debugger.command.simple.AbortCommand;
import ist.meic.pa.debugger.command.simple.GetCommand;
import ist.meic.pa.debugger.command.simple.InfoCommand;
import ist.meic.pa.debugger.command.simple.RetryCommand;
import ist.meic.pa.debugger.command.simple.ReturnCommand;
import ist.meic.pa.debugger.command.simple.SetCommand;
import ist.meic.pa.debugger.command.simple.ThrowCommand;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Scanner;

public final class DInterfaceSimple extends DInterface {

	private final static Scanner sc = new Scanner(System.in);
	private final static CommandManager commandsManager = new CommandManager(new Command[]{
		new AbortCommand(),
		new GetCommand(), 
		new InfoCommand(), 
		new RetryCommand(),
		new SetCommand(), 
		new ThrowCommand(), 
		new ReturnCommand()
	});
	
	@Override
	protected Object invokeMethodWithDebug(Class<?> targetClass, Object target, Method callingMethod, Object args[]) throws Throwable{
		while (true) {
			try {
				return callingMethod.invoke(target, args);
			} catch (InvocationTargetException e) {
				Command command = debugMethod(e.getTargetException(), targetClass, target);
				if (command.isReturnable()) {
					return command.getResult();
				}
			}
		}
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

				if (c.shouldExitDebugger()) {
					return c;
				}
				
			} catch (CommandException e) {
				System.err.println("DEBUGGER ERROR : " + e);
			}
		}
	}

}