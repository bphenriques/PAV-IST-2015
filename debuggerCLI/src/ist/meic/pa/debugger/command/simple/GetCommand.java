package ist.meic.pa.debugger.command.simple;

import ist.meic.pa.command.common.ClassUtil;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.Command;

import java.lang.reflect.Field;

/**
 * The GetCommand class is a command for use in the debugger, representing the
 * user "Get &lt;field&gt;" command.
 * <p>
 * Retrieves the value of the field &lt;field&gt;.
 * 
 */
public class GetCommand extends Command {

	/** The Constant COMMAND_NAME. */
	private static final String COMMAND_NAME = "Get";

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[], java.lang.Throwable, java.lang.Class)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {

		if (args.length != 2)
			throw new WrongNumberOfArgumentsException(1, args.length - 1);

		try {
			Field targetField = ClassUtil.getDeclaredField(targetClass,
					args[1]);
			System.out.println(ClassUtil.getFieldObject(null, targetField));

		} catch (IllegalAccessException | IllegalArgumentException
				| SecurityException | NoSuchFieldException e) {
			throw new CommandException(e.toString());
		}
	}

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[], java.lang.Throwable, java.lang.Object)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Object target)
			throws CommandException, Throwable {

		if (args.length != 2)
			throw new WrongNumberOfArgumentsException(1, args.length - 1);

		try {
			Class<?> targetClass = target.getClass();
			Field targetField = ClassUtil.getDeclaredField(targetClass,
					args[1]);
			System.out.println(ClassUtil.getFieldObject(target, targetField));

		} catch (IllegalAccessException | IllegalArgumentException
				| SecurityException | NoSuchFieldException e) {
			throw new CommandException(e.toString());
		}
	}

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#getCommandName()
	 */
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}