package de.chandre.admintool.db;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

/**
 * data source service interface
 * @author Andre
 *
 */
public interface AdminToolDBBrowserService 
{
	/**
	 * @return all data sources associated with spring context
	 */
	List<String> getDatasourceNames();
	
	/**
	 * gets a connection from datasource and sets autoCommit to false and readonly to true if DML is not allowed
	 * 
	 * @param datasourceName
	 * @param vars
	 * @return
	 * @throws SQLException
	 */
	Connection getConnection(String datasourceName, ConnectionVars vars) throws SQLException;

	/**
	 * close the connection and does a rollback if DML is not allowed. furthermore original values for autoCommit and readonly will be set.
	 * 
	 * @param c
	 * @param vars
	 */
	void closeConnection(Connection c, ConnectionVars vars);

	/**
	 * queries database with specified statementTO and returns the QueryResultTO
	 * 
	 * @param statementTO
	 * @return
	 */
	QueryResultTO queryDatabase(StatementTO statementTO);
}
