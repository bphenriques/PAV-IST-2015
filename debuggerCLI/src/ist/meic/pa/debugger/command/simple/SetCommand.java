package ist.meic.pa.debugger.command.simple;

import ist.meic.pa.command.common.ClassUtil;
import ist.meic.pa.command.common.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.NonPrimitiveSetException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;
import ist.meic.pa.debugger.command.Command;

import java.lang.reflect.Field;

/**
 * The SetCommand class is a command for use in the debugger, representing the
 * user "Set &lt;field&gt; &lt;value&gt;" command.
 * <p>
 * Changes the value of primitive typed field &lt;field&gt; to the user given
 * value of &lt;value&gt;.<br>
 * Internally, it interprets the String value and converts it to the
 * corresponding primitive value.
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
			if(!typeField.isPrimitive())
				throw new NonPrimitiveSetException();
			ObjectContructorFromString c = new ObjectContructorFromString();
			Object targetObj = c.convert(typeField, toValue);

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
			if(!typeField.isPrimitive())
				throw new NonPrimitiveSetException();
			ObjectContructorFromString c = new ObjectContructorFromString();
			Object targetObj = c.convert(typeField, toValue);

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
