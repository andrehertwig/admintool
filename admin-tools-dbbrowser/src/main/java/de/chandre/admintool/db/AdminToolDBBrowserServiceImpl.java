package de.chandre.admintool.db;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

/**
 * 
 * @author Andre
 *
 */
@Service("adminToolDBBrowserService")
public class AdminToolDBBrowserServiceImpl implements AdminToolDBBrowserService
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolDBBrowserServiceImpl.class);
	
	private static final String DEFAULT_CLOB_ENCODING = "UTF-8";
	
	private static final String META_KEY_DB_VERSION = "databaseProductVersion";
	private static final String META_KEY_DRIVER_VERSION = "driverVersion";
	private static final String META_KEY_DRIVER_NAME = "driverName";
	
	@Autowired
	private ApplicationContext applicationContext;
	
	@Autowired
	private AdminToolDBBrowserConfig configuration;
	
	@Autowired
	private Map<String, DataSource> datasources;
	
	@Autowired
	private AdminToolDBBrowserExampleLoader exampleLoader;
	
	
	
	/**
	 * @return the datasources
	 */
	@Override
	public Map<String, DataSource> getDatasources() {
		return datasources;
	}

	/**
	 * @param datasourceNames the datasourceNames to set
	 */
	@Override
	public void setDatasources(Map<String, DataSource> datasources) {
		if (LOGGER.isDebugEnabled()) 
			LOGGER.debug("receiving " + (null != datasources ? datasources.size() : "null") + " datasources");
		this.datasources = datasources;
	}

	@Override
	public List<String> getDatasourceNames() {
		if (null == datasources) {
			this.datasources = applicationContext.getBeansOfType(DataSource.class);
		}
		List<String> result = new ArrayList<>(datasources.keySet());
		Collections.sort(result);
		return result;
	}
	
	protected boolean isDMLAllowed() {
		return configuration.isDmlAllowed();
	}
	
	@Override
	public Connection getConnection(String datasourceName, ConnectionVars vars) throws SQLException {
		DataSource ds = datasources.get(datasourceName);
		if (null == ds) {
			throw new IllegalArgumentException("no datasource with name '" + datasourceName + "' found");
		}
		Connection c = ds.getConnection();
		if (!isDMLAllowed() && null != vars) {
			if (LOGGER.isDebugEnabled()) 
				LOGGER.debug("DML is not allowed. Set autoCommit to false and readOnly to true.");
			vars.setOrgAutoCommitState(c.getAutoCommit());
			vars.setOrgReadOnlyState(c.isReadOnly());
			c.setAutoCommit(false);
	        c.setReadOnly(true);
		}
		return c;
	}
	
	@Override
	public void closeConnection(Connection c, ConnectionVars vars) {
		if (!isDMLAllowed() && c != null && null != vars) {
			try {
				c.rollback();
				c.setAutoCommit(vars.isOrgAutoCommitState());
		    	c.setReadOnly(vars.isOrgReadOnlyState());
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
		//unsure about if we could close a pooled connection??
		//... but if we close the connection, setAutoCommit and setReadOnly is useless 
		if (null != c) {
			try {
				c.close();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
	}
	
	@Override
	public QueryResultTO getMetadata(String datasourceName) {
		if(!configuration.isEnabled()) return null;

		QueryResultTO resultTO = new QueryResultTO();
		Connection c = null;
		try {
			c = getConnection(datasourceName, null);
			DatabaseMetaData metadata = c.getMetaData();
			
			ResultSet resSet = metadata.getTables(null, null, null, null);
			StatementTO statementTO = new StatementTO();
			statementTO.setShowClobs(true);
			
			iterateResult(resSet, resultTO, statementTO);
			
			int colTableName = -1;
			int colSchemaName = -1;
			int i = 0;
			for (String colName : resultTO.getColumnsNames()) {
				if (colName.toLowerCase(Locale.ENGLISH).startsWith("table_name")) {
					colTableName = i;
	    		}
	    		if (colName.toLowerCase(Locale.ENGLISH).startsWith("table_sche")) {
	    			colSchemaName = i;
	    		}
	    		++i;
			}
			
			Map<String, Collection<String>> tablesAndColums = new HashMap<>();
			
			for (List<String> row : resultTO.getTableResult()) {
				String schemaName = colSchemaName > -1 ? row.get(colSchemaName) : null;
				String tableName = colTableName > -1 ? row.get(colTableName) : null;
				
				ResultSet tableResSet = metadata.getColumns(null, schemaName, tableName, null);
				
				QueryResultTO tableResultTO = new QueryResultTO();
				iterateResult(tableResSet, tableResultTO, statementTO);
				
				int colColumnName = -1;
				i = 0;
				for (String colName : tableResultTO.getColumnsNames()) {
		    		if (colName.toLowerCase(Locale.ENGLISH).startsWith("column_name")) {
		    			colColumnName = i;
		    		}
		    		++i;
				}
				
				Set<String> colums = new HashSet<>();
				
				for (List<String> colRow : tableResultTO.getTableResult()) {
					String columnName = colColumnName > -1 ? colRow.get(colColumnName) : null;
					if (null != columnName) {
						colums.add(columnName);
					}
				}
				if (tablesAndColums.containsKey(tableName)) {
					LOGGER.info(String.format("duplicate table found: %s (schema is: %s)", tableName, schemaName));
				}
				tablesAndColums.put(tableName, colums);
			}
			
			resultTO.setTableResult(null);
			
			resultTO.addMetadata("tables", tablesAndColums);
			
			resultTO.addMetadata(META_KEY_DB_VERSION, metadata.getDatabaseProductVersion());
			resultTO.addMetadata(META_KEY_DRIVER_VERSION, metadata.getDriverVersion());
			resultTO.addMetadata(META_KEY_DRIVER_NAME, metadata.getDriverName());
			
		} catch (Exception e) {
			resultTO.setExceptionMessage(e.getMessage());
			resultTO.setExceptionCause(null != e.getCause() ? e.getCause().toString() : null);
			resultTO.setExceptionTrace(printException(e));
			
		} finally {
			closeConnection(c, null);
		}
		
		if (LOGGER.isTraceEnabled()) {
			LOGGER.trace(resultTO);
		}
		return resultTO;
	}
	
	private String prepareQuery(StatementTO statementTO){
		String strQuery = statementTO.getStatement();
		
		if (statementTO.isBaseEncoded()) {
			strQuery = new String(Base64.decodeBase64(strQuery));
		}
		
		if (!strQuery.toLowerCase().contains("begin") && !strQuery.toLowerCase().contains("end;") && strQuery.endsWith(";"))
	    {
        	//if no PL-SQL statement trim the ";"
	        strQuery = strQuery.substring(0, strQuery.length() -1);
	    }
		return strQuery;
	}
	
	@Override
	public QueryResultTO queryDatabase(StatementTO statementTO) {
		if(!configuration.isEnabled()) return null;
		if (LOGGER.isTraceEnabled()) {
			LOGGER.trace(statementTO);
		}
		
		ConnectionVars vars = new ConnectionVars();
		QueryResultTO resultTO = new QueryResultTO(statementTO);
		if (StringUtils.isEmpty(statementTO.getStatement())) {
			return resultTO;
		}
		
		Connection c = null;
		try {
			c = getConnection(statementTO.getDatasourceName(), vars);
			Statement st = c.createStatement();
	        if (0 < statementTO.getMaxResults()) st.setMaxRows(statementTO.getMaxResults());
	        
	        String strQuery = prepareQuery(statementTO);
	        LOGGER.trace("executing query: " + strQuery);
			
	        if (strQuery.toLowerCase().startsWith("select")) {
	        	ResultSet resSet = null;
	        	resSet = st.executeQuery(strQuery);
	        	
	        	iterateResult(resSet, resultTO, statementTO);
		        
	        } else {
	        	resultTO.setAffectedRows(st.executeUpdate(strQuery));
	        	resultTO.setSelect(false);
	        }
		} catch (Exception e) {
			resultTO.setExceptionMessage(e.getMessage());
			resultTO.setExceptionCause(null != e.getCause() ? e.getCause().toString() : null);
			resultTO.setExceptionTrace(printException(e));
			
		} finally {
			closeConnection(c, vars);
		}
		
		if (LOGGER.isTraceEnabled()) {
			LOGGER.trace(resultTO);
		}
		
		return resultTO;
	}
	
	/**
	 * iterates the resultSet and fills resultTO
	 * @param resSet
	 * @param resultTO
	 * @param statementTO
	 */
	protected void iterateResult(ResultSet resSet, QueryResultTO resultTO, StatementTO statementTO) {
		
		try {
			if (resSet != null && !resSet.isClosed()) {
	    		ResultSetMetaData metaData = resSet.getMetaData();
			    int cols = metaData.getColumnCount();
			    Map<Integer, Integer> type = new HashMap<Integer, Integer>();
			    
			    List<String> columnsNames = new ArrayList<>();
			    
			    for (int i = 1; i < (cols+1); i++)
			    {
			    	String colName = metaData.getColumnName(i);
			    	columnsNames.add(colName);
			    	type.put(i, metaData.getColumnType(i));
			    }
			   
			    List<List<String>> tableResult = new ArrayList<>();
			    String clobEncoding = statementTO.getClobEncoding() != null ? statementTO.getClobEncoding() : DEFAULT_CLOB_ENCODING;
		        while (resSet.next())
			    {
		        	List<String> row = new ArrayList<>();
		        	for (int i = 1; i < (cols+1); i++)
		    	    {
		            	if (type.get(i) != null && type.get(i) == Types.BLOB) {
		            		if (statementTO.isShowBlobs()) {
		            			row.add(String.valueOf(new String(resSet.getBytes(i))));
			                } else {
			                	row.add(String.valueOf(resSet.getObject(i)));
			                }
		            	} else if (type.get(i) != null && type.get(i) == Types.CLOB) {
		            		if (statementTO.isShowClobs()) {
		            			row.add(getClobString(resSet.getClob(i), clobEncoding ));
		            		} else {
		            			row.add("CLOB content");
		            		}
		            	} else {
		            		row.add(String.valueOf(resSet.getObject(i)));
		            	}
		    	    }
		        	tableResult.add(row);
			    }
		        
		        resultTO.setSqlWarnings(null != resSet.getWarnings() ? resSet.getWarnings().toString() : null);
		        
		        resultTO.setAffectedRows(tableResult.size());
		        resultTO.setColumnsNames(columnsNames);
		        resultTO.setTableResult(tableResult);
	    	} else {
	    		resultTO.setSqlWarnings("resultSet was " + (null != resSet ? "closed already" : "null"));
	    	}
	        resultTO.setSelect(true);
	        
		} catch (Exception e) {
			resultTO.setExceptionMessage(e.getMessage());
			resultTO.setExceptionCause(null != e.getCause() ? e.getCause().toString() : null);
			resultTO.setExceptionTrace(printException(e));
		}
	}
	

	/**
	 * turns clob into a string
	 * 
	 * @param clobObject
	 * @param encoding
	 * @return
	 * @throws IOException
	 * @throws SQLException
	 * @throws UnsupportedEncodingException
	 */
	protected String getClobString(Clob clobObject, String encoding) throws IOException, SQLException, UnsupportedEncodingException
	{
	    if (null == clobObject) {
	        return "";
	    }
	    InputStream in = clobObject.getAsciiStream();
	    Reader read = new InputStreamReader(in, encoding);
	    StringWriter write = new StringWriter();
		String result = null;
	    try {
	        int c = -1;
		    while ((c = read.read()) != -1) {
		        write.write(c);
		    }
		    write.flush();
		    result = write.toString();
	    } finally {
	   	    closeStream(write);
	   	    closeStream(read);
	   	    //should we close the ascii stream from database? or is it handled by connection
	   	    // closeStream(in);
	   	}
	    return result;
	}
	
	/**
	 * prints a exception into a string
	 * @param throwable
	 * @return
	 */
	protected static String printException(final Throwable throwable)
    {
        if (null == throwable) {
            return null;
        }
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final PrintStream printStream = new PrintStream(baos);
        throwable.printStackTrace(printStream);
        String exceptionStr = "";
        try {
            exceptionStr = baos.toString("UTF-8");
        } catch (Exception ex) {
            exceptionStr = "Unavailable";
        } finally {
            closeStream(printStream);
            closeStream(baos);
        }
        return exceptionStr;
    }
	
	/**
	 * closes a closeable quietly
	 * @param closeable
	 */
	protected static void closeStream(final Closeable closeable)
	{
	    if (null != closeable) {
	        try {
	            closeable.close();
	        } catch (Exception ignore) {}
	    }
	}
	
	@Override
	public String getTab(StatementTO statementTO, String id) {
		return null != statementTO ? (id + "_" + String.valueOf(statementTO.getTab())) : (id + "_1");
	}
	
	@Override
	public Map<String, List<ExampleStatement>> getExamplesForDatasource(StatementTO statementTO) {
		if (null == exampleLoader.getExamples() || exampleLoader.getExamples().isEmpty()) {
			return null;
		}
		String dataSourceName = this.datasources.keySet().iterator().next();
		if (null != statementTO) {
			dataSourceName = statementTO.getDatasourceName();
		}
		return exampleLoader.getExamples().get(dataSourceName);
	}
	
}
