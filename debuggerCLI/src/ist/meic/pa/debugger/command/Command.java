package ist.meic.pa.debugger.command;

import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.command.exception.NonReturnableCommandException;

public abstract class Command {
	
	//Execute static methods
	public abstract void execute(String[] args, Throwable exception) throws CommandException, Throwable;
	
	//Execute non-static methods
	public abstract void execute(String[] args, Throwable exception, Object target) throws CommandException, Throwable;
	
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
