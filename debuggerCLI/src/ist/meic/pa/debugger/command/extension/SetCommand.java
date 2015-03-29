package ist.meic.pa.debugger.command.extension;

import ist.meic.pa.command.common.ClassUtil;
import ist.meic.pa.command.common.extension.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.Command;

import java.lang.reflect.Field;


/**
 * The Class SetCommand also sets non-primitives types.
 */
public class SetCommand extends Command {

	/** The Constant COMMAND_NAME. */
	private static final String COMMAND_NAME = "Set";

	/* (non-Javadoc)
	 * @see ist.meic.pa.debugger.command.Command#execute(java.lang.String[], java.lang.Throwable, java.lang.Class)
	 */
	@Override
	public void execute(String[] args, Throwable exception, Class<?> targetClass)
			throws CommandException, Throwable {

		if (args.length != 3)
			throw new WrongNumberOfArgumentsException(2, args.length - 1);

		String fieldName = args[1];
		String toValue = args[2];

		try {
			Field targetField = ClassUtil.getDeclaredField(targetClass,
					fieldName);

			Class<?> typeField = targetField.getType();
			ObjectContructorFromString c = new ObjectContructorFromString(
					typeField, toValue);
			Object targetObj = c.convert();

			ClassUtil.setFieldObject(null, targetField, targetObj);

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

		if (args.length != 3)
			throw new WrongNumberOfArgumentsException(2, args.length - 1);

		String fieldName = args[1];
		String toValue = args[2];

		try {

			Field targetField = ClassUtil.getDeclaredField(target.getClass(),
					fieldName);

			Class<?> typeField = targetField.getType();
			ObjectContructorFromString c = new ObjectContructorFromString(
					typeField, toValue);
			Object targetObj = c.convert();

			ClassUtil.setFieldObject(target, targetField, targetObj);

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
