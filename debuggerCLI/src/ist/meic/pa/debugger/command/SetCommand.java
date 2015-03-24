package ist.meic.pa.debugger.command;

import ist.meic.pa.command.common.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;

import java.lang.reflect.Field;

public class SetCommand extends Command {

	private static final String COMMAND_NAME = "Set";

	@Override
	public void execute(String[] args, Exception exception) throws CommandException {
		//FIXME FIXME FIXME FIXME
	}
	
	@Override
	public void execute(String[] args, Exception exception, Object target) throws CommandException {
		try {
			if (args.length != 3)
				throw new WrongNumberOfArgumentsException(1, args.length);
			
			Class<?> targetClass = target.getClass();
		
			String fieldName = args[1];
			String toValue = args[2];
			
			Field targetField = targetClass.getDeclaredField(fieldName);
			
			Class<?> valueClass = targetField.getType();
			
			boolean wasAccessible = targetField.isAccessible();
			
			targetField.setAccessible(true);
		
			ObjectContructorFromString c = new ObjectContructorFromString();
			Object targetObj = c.convert(valueClass, toValue);
			
			targetField.set(target, targetObj);
			
			if (wasAccessible)
				targetField.setAccessible(false);
			
		} catch (IllegalAccessException | IllegalArgumentException | 
				SecurityException | NoSuchFieldException e) {
			throw new CommandException(e.toString());
		}

	}
	
	@Override
	public String getCommandName() {
		return COMMAND_NAME;
	}

}
