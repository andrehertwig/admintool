package de.chandre.admintool.db;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.XML;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * 
 * @author Andre
 *
 */
@Component
public class AdminToolDBBrowserExampleLoader 
{
	private static final Log LOGGER = LogFactory.getLog(AdminToolDBBrowserExampleLoader.class);
	
	private HashMap<String, Map<String, List<ExampleStatement>>> statements = new HashMap<>();
	
	/**
	 * vendor must be set
	 * 
	 * @param exampleStatements
	 */
	public void addExamples(ExampleStatements exampleStatements)
    {
		this.statements.put(exampleStatements.getDatasourceName(), exampleStatements.getClusters());
		if (LOGGER.isDebugEnabled()) {
        	LOGGER.debug("converted json object" + new JSONObject(statements));
        }
    }
	
	public HashMap<String, Map<String, List<ExampleStatement>>> getExamples() {
		return this.statements;
	}
	
	
	/**
	 * datasourceName must be set<br>
	 * <br>
     *  Example:<br>
     * <code>
     * {<br>
     * &nbsp;&nbsp;		"datasourceName" : "datasource",<br>
     * &nbsp;&nbsp;		"cluster" : {<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;		"Maintainance" : [{<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;		"statement": "select 1",<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;		"description": "check select"<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;		}, {<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;		"statement": "select ...",<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;		"description": "..."<br>
     * &nbsp;&nbsp;&nbsp;&nbsp;		}]<br>
     * &nbsp;&nbsp;		}<br>
     * }<br>
     * </code>
     * @param jsonString
	 * @throws IOException 
	 * @throws JsonMappingException 
	 * @throws JsonParseException 
     * @see ExampleStatements
     */
    public void loadExampleStatementsFromJsonString(String jsonString)
    		throws JsonParseException, JsonMappingException, IOException
    {
    	if (LOGGER.isTraceEnabled()) {
    		LOGGER.trace("Receiving json string: " + jsonString);
    	}
    	ObjectMapper mapper = new ObjectMapper();
    	ExampleStatements exampleStatements = mapper.readValue(jsonString, ExampleStatements.class);
    	addExamples(exampleStatements);
    }
    
    /**
     * 
     * @param jsonStream
     * @throws JsonParseException
     * @throws JsonMappingException
     * @throws IOException
     * @see {@link #loadExampleStatementsFromJsonString(String)}
     */
    public void loadExampleStatementsFromJsonString(InputStream jsonStream)
    		throws JsonParseException, JsonMappingException, IOException
    {
    	ObjectMapper mapper = new ObjectMapper();
    	ExampleStatements exampleStatements = mapper.readValue(jsonStream, ExampleStatements.class);
    	addExamples(exampleStatements);
    }
    
    /**
     * @see {@link #loadExampleStatementsFromJsonString(String)}
     * @param jsonFile
     * @throws JsonParseException
     * @throws JsonMappingException
     * @throws IOException
     */
    public void loadExampleStatementsFromJsonString(File jsonFile)
    		throws JsonParseException, JsonMappingException, IOException
    {
    	ObjectMapper mapper = new ObjectMapper();
    	ExampleStatements exampleStatements = mapper.readValue(jsonFile, ExampleStatements.class);
    	addExamples(exampleStatements);
    }
    
    /**
     * @see {@link #loadExampleStatementsFromJsonString(String)}
     * @param jsonFile
     * @throws JsonParseException
     * @throws JsonMappingException
     * @throws IOException
     */
    public void loadExampleStatementsFromJsonString(byte[] jsonFile)
    		throws JsonParseException, JsonMappingException, IOException
    {
    	ObjectMapper mapper = new ObjectMapper();
    	ExampleStatements exampleStatements = mapper.readValue(jsonFile, ExampleStatements.class);
    	addExamples(exampleStatements);
    }
    
    /**
     * @see {@link #loadExampleStatementsFromJsonString(String)}
     * @param jsonFile
     * @throws JsonParseException
     * @throws JsonMappingException
     * @throws IOException
     */
    public void loadExampleStatementsFromJsonString(URL jsonFile)
    		throws JsonParseException, JsonMappingException, IOException
    {
    	ObjectMapper mapper = new ObjectMapper();
    	ExampleStatements exampleStatements = mapper.readValue(jsonFile, ExampleStatements.class);
    	addExamples(exampleStatements);
    }
    
    /**
     * vendor must be set and a type of {@link Vendor}
     * @param xmlString
     * @throws JSONException 
	 * @throws IllegalArgumentException 
     * @throws IOException 
     * @throws JsonMappingException 
     * @throws JsonParseException 
     * @see ExampleStatements
     */
    public void loadExampleStatementsFromXMLString(String xmlString) 
    		throws JSONException, IllegalArgumentException, JsonParseException, JsonMappingException, IOException
    {
    	if (LOGGER.isTraceEnabled()) {
    		LOGGER.trace("Receiving json string: " + xmlString);
    	}
    	JSONObject jsonobject = XML.toJSONObject(xmlString);
    	String[] root = JSONObject.getNames(jsonobject);
    	if (null == root) {
    		throw new IllegalArgumentException("no root object in xml found");
    	}
    	if (root.length > 1) {
    		throw new IllegalArgumentException("more than one root objects found");
    	}
    	JSONObject statement = jsonobject.getJSONObject(root[0]);
    	loadExampleStatementsFromJsonString(statement.toString());
    }
}
