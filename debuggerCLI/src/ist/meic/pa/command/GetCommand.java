package ist.meic.pa.command;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.InvalidCommandOnStaticException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;

import java.lang.reflect.Field;

public class GetCommand extends Command {

	private static final String COMMAND_NAME = "Get";

	@Override
	public void execute(String[] args, Exception exception)
			throws CommandException {
		throw new InvalidCommandOnStaticException(this);
	}

	@Override
	public void execute(String[] args, Exception exception, Object target)
			throws CommandException {

		try {
			@SuppressWarnings("rawtypes")
			Class targetClass = target.getClass();
			if (args.length != 2)
				throw new WrongNumberOfArgumentsException(1, args.length);
			Field targetField = targetClass.getDeclaredField(args[1]);

			targetField.setAccessible(true);
			System.out.println(targetField.get(target));
			targetField.setAccessible(false);

			// FIXME: Maybe throwing a generic exception isn't the best idea...
		} catch (NoSuchFieldException e) {
			throw new CommandException(e.toString());
		} catch (SecurityException e) {
			throw new CommandException(e.toString());
		} catch (IllegalArgumentException e) {
			throw new CommandException(e.toString());
		} catch (IllegalAccessException e) {
			throw new CommandException(e.toString());
		}

	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
