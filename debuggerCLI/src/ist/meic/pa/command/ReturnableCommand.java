package ist.meic.pa.command;

public abstract class ReturnableCommand extends Command {

	protected Object _result;
	
	@Override
	public boolean isReturnable(){
		return true;
	}
	
	@Override
	public Object getResult(){
		return _result;
	} 
}
