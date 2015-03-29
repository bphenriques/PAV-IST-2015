package ist.meic.pa.debugger.command;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.NonReplaceArgumentsCommandException;
import ist.meic.pa.command.exception.NonReplaceMethodCommandException;
import ist.meic.pa.command.exception.NonReturnableCommandException;

import java.lang.reflect.Method;

/**
 * The abstract class Command provides an abstraction for the commands that the
 * debugger can run, making the program easier to expand and more modular.
 *
 */
public abstract class Command {

	// Execute static methods
	/**
	 * Execute.
	 *
	 * @param args the args
	 * @param exception the exception
	 * @param targetClass the target class
	 * @throws CommandException the command exception
	 * @throws Throwable the throwable
	 */
	public abstract void execute(String[] args, Throwable exception,
			Class<?> targetClass) throws CommandException, Throwable;

	// Execute non-static methods
	/**
	 * Execute.
	 *
	 * @param args the args
	 * @param exception the exception
	 * @param target the target
	 * @throws CommandException the command exception
	 * @throws Throwable the throwable
	 */
	public abstract void execute(String[] args, Throwable exception,
			Object target) throws CommandException, Throwable;

	/**
	 * Gets the command name.
	 *
	 * @return the command name
	 */
	public abstract String getCommandName();

	/**
	 * Checks if is returnable.
	 *
	 * @return true, if is returnable
	 */
	public boolean isReturnable() {
		return false;
	}

	/**
	 * Should exit debugger.
	 *
	 * @return true, if successful
	 */
	public boolean shouldExitDebugger() {
		return isReturnable() || isRetriable();
	}

	/**
	 * Checks if is replace method.
	 *
	 * @return true, if is replace method
	 */
	public boolean isReplaceMethod() {
		return false;
	}
	
	/**
	 * Checks if is replace arguments.
	 *
	 * @return true, if is replace arguments
	 */
	public boolean isReplaceArguments() {
		return false;
	}

	/**
	 * Checks if is retriable.
	 *
	 * @return true, if is retriable
	 */
	public boolean isRetriable() {
		return false;
	}

	/**
	 * Gets the result.
	 *
	 * @return the result
	 * @throws NonReturnableCommandException the non returnable command exception
	 */
	public Object getResult() throws NonReturnableCommandException {
		throw new NonReturnableCommandException(getCommandName());
	}

	/**
	 * Gets the method result.
	 *
	 * @return the method result
	 * @throws NonReplaceMethodCommandException the non replace method command exception
	 */
	public Method getMethodResult() throws NonReplaceMethodCommandException {
		throw new NonReplaceMethodCommandException(getCommandName());
	}
	
	/**
	 * Gets the arguments result.
	 *
	 * @return the arguments result
	 * @throws NonReplaceArgumentsCommandException the non replace arguments command exception
	 */
	public Object[] getArgumentsResult() throws NonReplaceArgumentsCommandException {
		throw new NonReplaceArgumentsCommandException(getCommandName());
	}
}
