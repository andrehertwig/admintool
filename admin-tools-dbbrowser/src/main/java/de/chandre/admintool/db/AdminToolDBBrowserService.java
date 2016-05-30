package de.chandre.admintool.db;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

/**
 * database browser service interface
 * 
 * @author Andre
 *
 */
public interface AdminToolDBBrowserService 
{
	/**
	 * returns the configured data sources<br>
	 * could be used to add data sources manually
	 * @return Map&lt;"dataSourceName", DataSource&gt;
	 */
	Map<String, DataSource> getDatasources();
	
	/**
	 * to override the configured data sources
	 * @param datasources
	 */
	void setDatasources(Map<String, DataSource> datasources);
	
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
	 * 
	 * @param datasourceName
	 * @return some datasource metdata
	 */
	QueryResultTO getMetadata(String datasourceName);

	/**
	 * queries database with specified statementTO and returns the QueryResultTO
	 * 
	 * @param statementTO
	 * @return
	 */
	QueryResultTO queryDatabase(StatementTO statementTO);

	/**
	 * 
	 * @param statementTO
	 * @param id html id value for element
	 * @return <i>id</i>_statementTO.tab or <i>id</i>_1 as default
	 */
	String getTab(StatementTO statementTO, String id);

	/**
	 * returns the clusters of examples for vendor. if statementTO is null first data source will be used.
	 * @param statementTO
	 * @return
	 */
	Map<String, List<ExampleStatement>> getExamplesForDatasource(StatementTO statementTO);

}
