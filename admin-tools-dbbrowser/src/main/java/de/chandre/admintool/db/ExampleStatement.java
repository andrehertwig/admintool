package de.chandre.admintool.db;

import java.io.Serializable;

/**
 * Example statements for DatabaseBrowser
 * @author Andre
 *
 */
public class ExampleStatement implements Serializable
{
	private static final long serialVersionUID = 1L;
	
	private String statement;
	private String description;
	
	/**
	 * Creates a new ExampleStatement
	 */
	public ExampleStatement() {
	}
	
	/**
	 * Creates a new ExampleStatement instance using fields
	 * 
	 * @param statement
	 * @param description
	 */
	public ExampleStatement(String statement, String description) {
		super();
		this.statement = statement;
		this.description = description;
	}



	/**
	 * @return the statement
	 */
	public String getStatement() {
		return statement;
	}
	
	/**
	 * the sql statement
	 * @param statement the statement to set
	 */
	public void setStatement(String statement) {
		this.statement = statement;
	}
	
	/**
	 * @return the description
	 */
	public String getDescription() {
		return description;
	}
	
	/**
	 * a description what the statement is for, or what it should return
	 * @param description the description to set
	 */
	public void setDescription(String description) {
		this.description = description;
	}
	
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ExampleStatement [statement=").append(statement).append(", description=").append(description)
				.append("]");
		return builder.toString();
	}
}
