package ist.meic.pa.command;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;

public class SetCommand extends Command {

	private static final String COMMAND_NAME = "Set";

	@Override
	public void execute(String[] args, Exception exception) throws CommandException {
		// TODO Auto-generated method stub
	}
	
	@Override
	public void execute(String[] args, Exception exception, Object target) throws CommandException {
		try {
			@SuppressWarnings("rawtypes")
			Class targetClass = target.getClass();
			if (args.length != 3)
				throw new WrongNumberOfArgumentsException(1, args.length);
			
			String fieldName = args[1];
			String toValue = args[2];

			
			Field targetField = targetClass.getDeclaredField(fieldName);
			
			Class<?> valueClass = target.getClass();
			Constructor<?> c = valueClass.getConstructor(String.class);
			Object value = c.newInstance(toValue);
			
			boolean wasAccessible = targetField.isAccessible();
			
			targetField.setAccessible(false);
			targetField.set(target, value);
			
			if (wasAccessible)
				targetField.setAccessible(true);
			
		} catch (InvocationTargetException | InstantiationException | 
				IllegalAccessException | IllegalArgumentException | 
				SecurityException | NoSuchFieldException | NoSuchMethodException e) {
			throw new CommandException(e.toString());
		}

	}
	
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
