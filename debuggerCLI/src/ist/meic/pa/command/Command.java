package ist.meic.pa.command;

public abstract class Command {
	//FIXME: It shouldn't throw so many exceptions.
	public abstract void execute(String[] args, Exception exception) throws CommandException;
	public abstract void execute(String[] args, Exception exception, Object target) throws CommandException;
	public abstract String getCommandName();
	
	public boolean isReturnable(){
		return false;
	}
	
	public Object getResult() throws NonReturnableCommandException{
		throw new NonReturnableCommandException(getCommandName());
	}
}
