package ist.meic.pa.debugger.command;

import ist.meic.pa.command.common.Finder;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.InvalidCommandOnStaticException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;

import java.lang.reflect.Field;

public class GetCommand extends Command {

	private static final String COMMAND_NAME = "Get";

	@Override
	public void execute(String[] args, Exception exception)
			throws CommandException {
		//FIXME FIXME
		throw new InvalidCommandOnStaticException(this);
	}

	@Override
	public void execute(String[] args, Exception exception, Object target)
			throws CommandException {

		try {
			Class<?> targetClass = target.getClass();
			if (args.length != 2)
				throw new WrongNumberOfArgumentsException(1, args.length);
			Field targetField = Finder.getField(targetClass, args[1]);

			boolean previousAccessiblValue = targetField.isAccessible();
			targetField.setAccessible(true);
			System.out.println(targetField.get(target));
			targetField.setAccessible(previousAccessiblValue);

			// FIXME: Maybe throwing a generic exception isn't the best idea...
		} catch (IllegalAccessException | IllegalArgumentException | SecurityException | NoSuchFieldException e) {
			throw new CommandException(e.toString());
		}

	}

	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}