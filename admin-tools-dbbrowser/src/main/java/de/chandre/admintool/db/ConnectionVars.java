package de.chandre.admintool.db;

/**
 * internal class to hold variables to control database connection within the request<br>
 * if DML is not allowed the AutoCommit and ReadOnly configuration should set back to original values 
 * 
 * @author Andre
 *
 */
public class ConnectionVars 
{
	private boolean orgAutoCommitState;
	private boolean orgReadOnlyState;
	
	/**
	 * @return the orgAutoCommitState
	 */
	public boolean isOrgAutoCommitState() {
		return orgAutoCommitState;
	}
	/**
	 * @param orgAutoCommitState the orgAutoCommitState to set
	 */
	public void setOrgAutoCommitState(boolean orgAutoCommitState) {
		this.orgAutoCommitState = orgAutoCommitState;
	}
	/**
	 * @return the orgReadOnlyState
	 */
	public boolean isOrgReadOnlyState() {
		return orgReadOnlyState;
	}
	/**
	 * @param orgReadOnlyState the orgReadOnlyState to set
	 */
	public void setOrgReadOnlyState(boolean orgReadOnlyState) {
		this.orgReadOnlyState = orgReadOnlyState;
	}
	
	
}
