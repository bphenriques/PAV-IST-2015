package ist.meic.pa.debugger.command;

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
