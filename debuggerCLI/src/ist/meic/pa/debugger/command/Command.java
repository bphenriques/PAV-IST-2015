package ist.meic.pa.debugger.command;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.NonReturnableCommandException;

public abstract class Command {
	
	//Execute static methods
	public abstract void execute(String[] args, Exception exception) throws CommandException, Exception;
	
	//Execute non-static methods
	public abstract void execute(String[] args, Exception exception, Object target) throws CommandException, Exception;
	
	public abstract String getCommandName();
	
	public boolean isReturnable(){
		return false;
	}
	
	public boolean isRetriable(){
		return false;
	}
	
	
	public Object getResult() throws NonReturnableCommandException{
		throw new NonReturnableCommandException(getCommandName());
	}
}
