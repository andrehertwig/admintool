package de.chandre.admintool.jminix;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanException;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.MBeanServer;
import javax.management.MBeanServerDelegate;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.ReflectionException;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import de.chandre.admintool.jmx.jstree.JmxExecuteTO;
import de.chandre.admintool.jmx.jstree.JmxMethodTO;
import de.chandre.admintool.jmx.jstree.JmxOperationTO;
import de.chandre.admintool.jmx.jstree.JmxQueryTO;
import de.chandre.admintool.jmx.jstree.JmxResponseTO;
import de.chandre.admintool.jmx.jstree.JsTree;
import de.chandre.admintool.jmx.jstree.JsTreeState;

@Service
public class AdminToolJmxServiceImpl implements AdminToolJmxService {
	
	private static final Log LOGGER = LogFactory.getLog(AdminToolJmxServiceImpl.class);
	
	private static final String ROLE = "role";
	private static final String GETTER = "getter";
	private static final String SERVER_ID = "MBeanServerId";
	
	private static final List<String> SIMPLE_TYPES = Arrays.asList("boolean", "byte", "short", "int", "long", "double", "float");
	
	private static String stringArraySeparator = ";";
	
	private Map<String, String> serverReferences = new HashMap<String, String>();
	
	@Autowired(required = false)
	private ObjectMapper objectMapper;
	
	@PostConstruct
	public void init() {
		try {
			ManagementFactory.getPlatformMBeanServer();
		} catch (Exception e) {
			LOGGER.error("no access to PlatformMBeanServer: " + e.getMessage());
		}
		
		try {
			int i = -1;
			for (String serverId : getServerIds()) {
				serverReferences.put(String.valueOf(++i), serverId);
			}
		} catch (Exception e) {
			LOGGER.error("could not retrieve serverIds: " + e.getMessage());
		}
		
		if (null == objectMapper) {
			LOGGER.warn("no objectMapper found, creating new one");
			objectMapper = new ObjectMapper();
		}
	}
	
	private List<MBeanServer> getServers() {
		List<MBeanServer> servers = MBeanServerFactory.findMBeanServer(null);
		return servers;
	}
	
	private List<String> getServerIds()  {
		List<MBeanServer> servers = getServers();
		
		List<String> serverIds = new ArrayList<>();
		for (MBeanServer mBeanServer : servers) {
			try {
				serverIds.add((String)mBeanServer.getAttribute(MBeanServerDelegate.DELEGATE_NAME, SERVER_ID));
			} catch (InstanceNotFoundException | AttributeNotFoundException | ReflectionException | MBeanException e) {
				LOGGER.error("could not retrieve serverId: " + e.getMessage());
			}
		}
		
		return serverIds;
	}
	
	private MBeanServer findServerByMappingKey(String serverMappingKey) {
		String agentId = serverReferences.get(serverMappingKey);
		if (null != agentId) {
			List<MBeanServer> servers =  MBeanServerFactory.findMBeanServer(agentId);
			if (null != servers && servers.size() == 1) {
				return servers.iterator().next();
			}
			throw new IllegalStateException("could not obtain correct server because mbean server list has a size of: " 
					+ (null != servers ?servers.size() : "null"));
		}
		throw new NullPointerException("could not obtain correct mbean server");
	}
	
	@Override
	public Map<String, String> getServerMappings() {
		return Collections.unmodifiableMap(serverReferences);
	}
	
	@Override
	public List<String> getDomains(String serverMappingKey) {
		MBeanServer server = findServerByMappingKey(serverMappingKey);
		List<String> domains = Arrays.asList(server.getDomains());
		Collections.sort(domains);
		
		return domains;
	}
	
	@Override
	public List<String> getMBeans(String serverMappingKey, String domain) {
		MBeanServer server = findServerByMappingKey(serverMappingKey);
		try {
			Set<ObjectName> names =server.queryNames(new ObjectName(domain+":*"), null);
			List<String> result = new ArrayList<String>();
            
            for(Object name : names) {
                result.add(name.toString().substring(domain.length() + 1));
            }
            Collections.sort(result);
            
            return result;
		} catch (MalformedObjectNameException e) {
			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}
	
	@Override
	public List<MBeanAttributeInfo> getAttributes(String serverMappingKey, String domain, String mbean) {
		MBeanServer server = findServerByMappingKey(serverMappingKey);
		
		try {
			List<MBeanAttributeInfo> result = new ArrayList<MBeanAttributeInfo>(
					Arrays.asList(server.getMBeanInfo(new ObjectName(domain+":"+mbean)).getAttributes()));
	        Iterator<MBeanAttributeInfo> i = result.iterator();
	        // Filters unreadable attributes
	        while(i.hasNext()) {
	            if(! i.next().isReadable()) {
	                i.remove();
	            }
	        }
	        Collections.sort(result, new Comparator<MBeanAttributeInfo>() {
	            public int compare(MBeanAttributeInfo o1, MBeanAttributeInfo o2)
	            {
	                return o1.getName().compareTo(o2.getName());
	            }                
	        });
	        return result;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}
	
	@Override
	public JmxResponseTO getAttributes(JmxQueryTO queryTo) {
		List<MBeanAttributeInfo> attributes = getAttributes(queryTo.getServer(), queryTo.getDomain(), queryTo.getMbean());
		
		JmxResponseTO response = new JmxResponseTO();
		for (MBeanAttributeInfo mBeanAttributeInfo : attributes) {
			boolean add = false;
			if (StringUtils.isEmpty(queryTo.getName())) {
				add = true;
			} else if (queryTo.getName().equals(mBeanAttributeInfo.getName())) {
				add = true;
			}
			JmxMethodTO method = new JmxMethodTO();
			method.setName(mBeanAttributeInfo.getName());
			method.setDescription(mBeanAttributeInfo.getDescription());
			method.setType(mBeanAttributeInfo.getType());
			method.setValue(getAttributeValue(queryTo, mBeanAttributeInfo.getName()));
			if (add) {
				response.addMethod(method);
			}
		}
		return response;
	}
	
	@Override
	public JmxResponseTO getOpperation(JmxQueryTO queryTo) {
		List<MBeanOperationInfo> opperations = getOperations(queryTo.getServer(), queryTo.getDomain(), queryTo.getMbean());
		JmxResponseTO response = new JmxResponseTO();
		for (MBeanOperationInfo mBeanOperationInfo : opperations) {
			if (null != queryTo.getName() && queryTo.getName().equals(mBeanOperationInfo.getName())) {
				JmxOperationTO method = new JmxOperationTO();
				method.setName(mBeanOperationInfo.getName());
				method.setDescription(mBeanOperationInfo.getDescription());
				method.setType(mBeanOperationInfo.getReturnType());
//				method.setValue(getAttributeValue(queryTo, mBeanOperationInfo.getName()));
				
				MBeanParameterInfo[] parameterInfo = mBeanOperationInfo.getSignature();
				for (MBeanParameterInfo mBeanParameterInfo : parameterInfo) {
					JmxMethodTO parameter = new JmxMethodTO();
					parameter.setName(mBeanParameterInfo.getName());
					parameter.setDescription(mBeanParameterInfo.getDescription());
					parameter.setType(mBeanParameterInfo.getType());
					parameter.setGeneralType(getGeneralType(mBeanParameterInfo.getType()));
					method.addParameter(parameter);
				}
				
				response.addMethod(method);
			}
		}
		return response;
	}
	
	@Override
	public JmxResponseTO getExecuteOperation(JmxExecuteTO queryTo) throws Exception {
		List<MBeanOperationInfo> opperations = getOperations(queryTo.getServer(), queryTo.getDomain(), queryTo.getMbean());
		JmxResponseTO response = getOpperation(queryTo);
		
		try {
			for (MBeanOperationInfo mBeanOperationInfo : opperations) {
				if (null != queryTo.getName() && queryTo.getName().equals(mBeanOperationInfo.getName())) {
					
					MBeanParameterInfo[] parameterInfo = mBeanOperationInfo.getSignature();
					Object[] values = new Object[parameterInfo.length];
					String[] signatures = new String[parameterInfo.length];
					int i = 0;
					for (MBeanParameterInfo mBeanParameterInfo : parameterInfo) {
						for (JmxMethodTO method: queryTo.getParameters()) {
							if (method.getName().equals(mBeanParameterInfo.getName())) {
								//found matching parameter
								signatures[i] = mBeanParameterInfo.getType();
								values[i] = parse(method.getNewValue(),  mBeanParameterInfo.getType());
								i++;
							}
						}
					}
					
					MBeanServer server = findServerByMappingKey(queryTo.getServer());
					Object result = server.invoke(
							new ObjectName(queryTo.getDomain()+":"+queryTo.getMbean()), 
							queryTo.getName(), values, signatures);
				}
			}
			response.setSuccess(Boolean.TRUE);
		} catch (ReflectionException | MBeanException | InstanceNotFoundException | MalformedObjectNameException e) {
			LOGGER.error(e.getMessage(), e);
			response.setSuccess(Boolean.FALSE);
		}
		
		return response;
	}
	
	protected Object getAttributeValue(JmxQueryTO queryTo, String attributeName) {
		try {
			
			String name = null == attributeName ? queryTo.getName() : attributeName;
			MBeanServer server = findServerByMappingKey(queryTo.getServer());
			Object value = server.getAttribute(new ObjectName(queryTo.getDomain()+":"+queryTo.getMbean()), name);
			
			if (null == value) {
				return "null";
			}
			
			return value;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		return "[error retrieving value]";
	}
	
	private String getGeneralType(String type) {
		if (type == null) {
			return type;
		}
		if (SIMPLE_TYPES.contains(type)) {
			return type;
		}
		try {
			Class<?> clazz = Class.forName(type);
			if(Collection.class.isAssignableFrom(clazz)) {
				return Collection.class.getName();
			}
			else if(Map.class.isAssignableFrom(clazz)) {
				return Map.class.getName();
			}
		} catch (ClassNotFoundException e) {
			LOGGER.trace(e.getMessage(), e);
		}
		return type;
	}
	
	public Object parse(String value, String type) throws JsonParseException, JsonMappingException, IOException, ClassNotFoundException {
		Object result = null;
		
		if(!SIMPLE_TYPES.contains(type)) {
			
			if (type.equals("[Ljava.lang.String;")) {
				result = StringUtils.splitPreserveAllTokens(value, stringArraySeparator);
			} else {
				Class<?> clazz = Class.forName(type);
				result = objectMapper.readValue(value, clazz);
			}
		}
		else if (StringUtils.isNotBlank(value)) {
			if (type.equals("byte")) {
				return Byte.valueOf(value).byteValue();
			}
			else if (type.equals("short")) {
				return Short.valueOf(value).shortValue();
			}
			else if (type.equals("int")) {
				return Integer.valueOf(value).intValue();
			}
			else if (type.equals("long")) {
				return Long.valueOf(value).longValue();
			}
			else if (type.equals("double")) {
				return Double.valueOf(value).doubleValue();
			}
			else if (type.equals("float")) {
				return Float.valueOf(value).floatValue();
			}
			else if (type.equals("boolean")) {
				return Boolean.valueOf(value).booleanValue();
			}
		}

		if (result == null) {
			throw new IllegalArgumentException("Type " + type + " with value " + value + " is not supported");
		}

		return result;
	}
	
	@Override
	public List<MBeanOperationInfo> getOperations(String serverMappingKey, String domain, String mbean) {
		MBeanServer server = findServerByMappingKey(serverMappingKey);
		
		try {
			List<MBeanOperationInfo> result = new ArrayList<MBeanOperationInfo>(
					Arrays.asList(server.getMBeanInfo(new ObjectName(domain+":"+mbean)).getOperations()));
			Iterator<MBeanOperationInfo> i = result.iterator();
			while(i.hasNext()) {
				
				MBeanOperationInfo info = i.next();
				Object role = info.getDescriptor().getFieldValue(ROLE);
	            if(null != role && info.getSignature().length == 0 || GETTER.equals(role)) {
	                i.remove();
	            }
	        }
			Collections.sort(result, new Comparator<MBeanOperationInfo>() {
			    public int compare(MBeanOperationInfo o1, MBeanOperationInfo o2)
			    {
			        return o1.getName().compareTo(o2.getName());
			    }                
			});		
			return result;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}
	
	private static final String US = "_";
	
	private static final String TYPE_SERVER = "server";
	private static final String TYPE_DOMAIN = "domain";
	private static final String TYPE_MBEAN = "mbean";
	private static final String TYPE_ATTRIBUTES = "attributes";
	private static final String TYPE_ATTRIBUTE = "attribute";
	private static final String TYPE_OPERATIONS = "operations";
	private static final String TYPE_OPERATION = "operation";
	
	@Override
	public List<JsTree> buildTree() {
		List<JsTree> result = new ArrayList<>();
		Map<String, String> serverMappings = getServerMappings();
		boolean open = true;
		for (String serverKey : serverMappings.keySet()) {
			JsTree root = new JsTree(serverKey, "Server[" + serverKey + "]", TYPE_SERVER);
			root.setState(new JsTreeState(open, false, false));
			
			for (String domain : getDomains(serverKey)) {
				JsTree node1 = new JsTree(domain, domain, TYPE_DOMAIN);
				
				for (String mbean : getMBeans(serverKey, domain)) {
//					String unescapedMbean = unescape(mbean);
					JsTree node2 = new JsTree(mbean, mbean, TYPE_MBEAN);
					
					List<MBeanAttributeInfo> ainfos = getAttributes(serverKey, domain, mbean);
					if (null != ainfos) {
						JsTree attrs = new JsTree(mbean + US + TYPE_ATTRIBUTES, "Attributes", TYPE_ATTRIBUTES);
						node2.addChildren(attrs);
						for (MBeanAttributeInfo ainfo : ainfos) {
							JsTree node3 = new JsTree(mbean + US + TYPE_ATTRIBUTE + US + ainfo.getName(), ainfo.getName(), TYPE_ATTRIBUTE);
							attrs.addChildren(node3);
						}
					}
					
					List<MBeanOperationInfo> oinfos = getOperations(serverKey, domain, mbean);
					if (null != oinfos) {
						JsTree ops = new JsTree(mbean + US + TYPE_OPERATIONS, "Operations", TYPE_OPERATIONS);
						node2.addChildren(ops);
						for (MBeanOperationInfo oinfo : oinfos) {
							JsTree node3 = new JsTree(mbean + US + TYPE_OPERATION + US + oinfo.getName(), oinfo.getName(), TYPE_OPERATION);
							ops.addChildren(node3);
						}
					}
					
					node1.addChildren(node2);
				}
				
				root.addChildren(node1);
			}
			
			result.add(root);
			open = false;
		}
		return result;
	}
	
	public String buildJson() throws JsonProcessingException {
		long start = System.currentTimeMillis();
		try {
			objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
			return objectMapper.writeValueAsString(buildTree());
		} finally {
			LOGGER.info("building json took " + (System.currentTimeMillis() - start) + "ms");
		}
	}
	
//	public String escape(String value) {
//        return value.replaceAll("/", "¦");
//    }
//
//    public String unescape(String value) {
//        return value.replaceAll("¦", "/");
//    }
	
}
