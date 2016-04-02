package de.chandre.admintool.db;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * transfer object for result of query 
 * @author Andre
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.PROPERTY)
public class QueryResultTO implements Serializable
{
	private static final long serialVersionUID = 1L;
	
	private StatementTO statement;
	
	private int affectedRows;
	private boolean select;
	
	private List<String> columnsNames;
	private List<List<String>> tableResult;
	
	private String sqlWarnings;
	
	private String exceptionMessage;
	private String exceptionCause;
	private String exceptionTrace;
	
	public QueryResultTO() {
	}
	
	public QueryResultTO(StatementTO statement) {
		this.statement = statement;
	}

	/**
	 * @return the statement
	 */
	public StatementTO getStatement() {
		return statement;
	}

	/**
	 * @param statement the statement to set
	 */
	public void setStatement(StatementTO statement) {
		this.statement = statement;
	}

	/**
	 * @return the affectedRows
	 */
	public int getAffectedRows() {
		return affectedRows;
	}

	/**
	 * @param affectedRows the affectedRows to set
	 */
	public void setAffectedRows(int affectedRows) {
		this.affectedRows = affectedRows;
	}

	/**
	 * @return the select
	 */
	public boolean isSelect() {
		return select;
	}

	/**
	 * @param select the select to set
	 */
	public void setSelect(boolean select) {
		this.select = select;
	}

	/**
	 * @return the columnsNames
	 */
	public List<String> getColumnsNames() {
		return columnsNames;
	}

	/**
	 * @param columnsNames the columnsNames to set
	 */
	public void setColumnsNames(List<String> columnsNames) {
		this.columnsNames = columnsNames;
	}

	/**
	 * @return the tableResult
	 */
	public List<List<String>> getTableResult() {
		return tableResult;
	}

	/**
	 * @param tableResult the tableResult to set
	 */
	public void setTableResult(List<List<String>> tableResult) {
		this.tableResult = tableResult;
	}

	/**
	 * @return the sqlWarnings
	 */
	public String getSqlWarnings() {
		return sqlWarnings;
	}

	/**
	 * @param sqlWarnings the sqlWarnings to set
	 */
	public void setSqlWarnings(String sqlWarnings) {
		this.sqlWarnings = sqlWarnings;
	}

	/**
	 * @return the exceptionMessage
	 */
	public String getExceptionMessage() {
		return exceptionMessage;
	}

	/**
	 * @param exceptionMessage the exceptionMessage to set
	 */
	public void setExceptionMessage(String exceptionMessage) {
		this.exceptionMessage = exceptionMessage;
	}

	/**
	 * @return the exceptionCause
	 */
	public String getExceptionCause() {
		return exceptionCause;
	}

	/**
	 * @param exceptionCause the exceptionCause to set
	 */
	public void setExceptionCause(String exceptionCause) {
		this.exceptionCause = exceptionCause;
	}

	/**
	 * @return the exceptionTrace
	 */
	public String getExceptionTrace() {
		return exceptionTrace;
	}

	/**
	 * @param exceptionTrace the exceptionTrace to set
	 */
	public void setExceptionTrace(String exceptionTrace) {
		this.exceptionTrace = exceptionTrace;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("QueryResultTO [statement=").append(statement).append(", affectedRows=").append(affectedRows)
				.append(", select=").append(select).append(", columnsNames=").append(columnsNames)
				.append(", tableResult=").append(tableResult).append(", sqlWarnings=").append(sqlWarnings)
				.append(", exceptionmessage=").append(exceptionMessage).append(", exceptioncause=")
				.append(exceptionCause).append(", exceptionTrace=").append(exceptionTrace).append("]");
		return builder.toString();
	}
}
