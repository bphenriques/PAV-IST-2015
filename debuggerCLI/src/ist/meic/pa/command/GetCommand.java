package ist.meic.pa.command;

import java.lang.reflect.Field;

public class GetCommand extends Command {

	private static final String NAME = "Get";
	
	@Override
	public void execute(String[] args, Exception exception) throws CommandException {
		throw new InvalidCommandOnStaticException(this);
	}
	
	@Override
	public void execute(String[] args, Exception exception, Object target)
			throws CommandException {

		try {
			@SuppressWarnings("rawtypes")
			Class targetClass = target.getClass();
			if (args.length == 2)
				throw new WrongNumberOfArgumentsException(1, args.length);
			Field targetField = targetClass.getField(args[1]);

			targetField.setAccessible(true);
			System.out.println(targetField.get(target));
			targetField.setAccessible(false);
			
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
		return NAME;
	}

}
