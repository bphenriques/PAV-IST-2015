package ist.meic.pa.debugger;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.debugger.command.AbortCommand;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.command.CommandManager;
import ist.meic.pa.debugger.command.GetCommand;
import ist.meic.pa.debugger.command.InfoCommand;
import ist.meic.pa.debugger.command.RetryCommand;
import ist.meic.pa.debugger.command.ReturnCommand;
import ist.meic.pa.debugger.command.SetCommand;
import ist.meic.pa.debugger.command.ThrowCommand;

import java.util.Scanner;

public final class DInterfaceSimple extends DInterface {

	public DInterfaceSimple(Class<?> targetClass, Object target,
			Class<?> returnType, String methodName, Class<?>[] parameterTypes,
			Object[] args) {
		super(targetClass, target, returnType, methodName, parameterTypes, args);
	}

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
	public void debugMethod(Throwable thrownException,
			Class<?> targetClass, Object target) throws Throwable {
		System.out.println(thrownException);

		while (true) {
			System.out.print("DebuggerCLI:> ");
			System.out.flush();

			String input = sc.nextLine();

			try {
				Command c = commandsManager.executeCommand(thrownException, input,
						targetClass, target);

				if (c.isReturnable()){
					_returnResult = c.getResult();
					break;
				}else if(c.isRetriable()){
					break;
				}
				
			} catch (CommandException e) {
				System.err.println("DEBUGGER ERROR : " + e);
			}
		}
	}

}
