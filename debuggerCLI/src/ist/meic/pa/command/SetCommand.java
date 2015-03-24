package ist.meic.pa.command;

import java.beans.PropertyEditor;
import java.beans.PropertyEditorManager;
import java.lang.reflect.Field;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.WrongNumberOfArgumentsException;

public class SetCommand extends Command {

	private static final String COMMAND_NAME = "Set";

	@Override
	public void execute(String[] args, Exception exception) throws CommandException {
		// TODO Auto-generated method stub
	}
	
	private Object convert(Class<?> targetType, String text) {
	    PropertyEditor editor = PropertyEditorManager.findEditor(targetType);
	    editor.setAsText(text);
	    return editor.getValue();
	}
	
	@Override
	public void execute(String[] args, Exception exception, Object target) throws CommandException {
		try {
			if (args.length != 3)
				throw new WrongNumberOfArgumentsException(1, args.length);
			
			@SuppressWarnings("rawtypes")
			Class targetClass = target.getClass();
		
			String fieldName = args[1];
			String toValue = args[2];
			
			Field targetField = targetClass.getDeclaredField(fieldName);
			
			Class<?> valueClass = targetField.getType();
			
			boolean wasAccessible = targetField.isAccessible();
			
			targetField.setAccessible(true);
			targetField.set(target, convert(valueClass, toValue));
			
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
