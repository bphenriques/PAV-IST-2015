package ist.meic.pa.debugger.command;

import ist.meic.pa.command.common.Finder;
import ist.meic.pa.command.common.ObjectContructorFromString;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;

import java.lang.reflect.Field;

public class SetCommand extends Command {

	private static final String COMMAND_NAME = "Set";

	@Override
	public void execute(String[] args, Throwable exception) throws CommandException {
		//FIXME FIXME FIXME FIXME
	}
	
	@Override
	public void execute(String[] args, Throwable exception, Object target) throws CommandException {
		try {
			if (args.length != 3)
				throw new WrongNumberOfArgumentsException(1, args.length);
			
			Class<?> targetClass = target.getClass();
		
			String fieldName = args[1];
			String toValue = args[2];
			
			Field targetField = Finder.getField(targetClass, fieldName);
			
			Class<?> valueClass = targetField.getType();
			
			boolean lastAccessibleValue = targetField.isAccessible();
			
			targetField.setAccessible(true);
		
			ObjectContructorFromString c = new ObjectContructorFromString();
			Object targetObj = c.convert(valueClass, toValue);
			
			targetField.set(target, targetObj);
			
			targetField.setAccessible(lastAccessibleValue);
			
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
