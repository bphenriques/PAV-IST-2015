package ist.meic.pa.debugger.command;

import ist.meic.pa.command.common.FieldFinder;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;

import java.lang.reflect.Field;

/**
 * The GetCommand class is a command for use in the debugger, representing the
 * user "Get &lt;field&gt;" command.
 * <p>
 * Retrieves the value of the field &lt;field&gt;.
 * 
 */
public class GetCommand extends Command {

	private static final String COMMAND_NAME = "Get";

	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {

		if (args.length != 2)
			throw new WrongNumberOfArgumentsException(1, args.length);

		try {
			Field targetField = FieldFinder.getDeclaredField(targetClass,
					args[1]);
			System.out.println(FieldFinder.getFieldObject(null, targetField));

		} catch (IllegalAccessException | IllegalArgumentException
				| SecurityException | NoSuchFieldException e) {
			throw new CommandException(e.toString());
		}
	}

	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {

		if (args.length != 2)
			throw new WrongNumberOfArgumentsException(1, args.length);

		try {
			Class<?> targetClass = target.getClass();
			Field targetField = FieldFinder.getDeclaredField(targetClass,
					args[1]);
			targetField.isAccessible();
			System.out.println(FieldFinder.getFieldObject(target, targetField));

		} catch (IllegalAccessException | IllegalArgumentException
				| SecurityException | NoSuchFieldException e) {
			throw new CommandException(e.toString());
		}
	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}