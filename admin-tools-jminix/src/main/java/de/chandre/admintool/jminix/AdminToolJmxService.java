package de.chandre.admintool.jminix;

import java.util.List;
import java.util.Map;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanOperationInfo;

import de.chandre.admintool.jmx.jstree.JmxExecuteTO;
import de.chandre.admintool.jmx.jstree.JmxQueryTO;
import de.chandre.admintool.jmx.jstree.JmxResponseTO;
import de.chandre.admintool.jmx.jstree.JsTree;

public interface AdminToolJmxService {

	Map<String, String> getServerMappings();

	List<String> getDomains(String serverMappingKey);

	List<String> getMBeans(String serverMappingKey, String domain);

	List<MBeanAttributeInfo> getAttributes(String serverMappingKey, String domain, String mbean);

	JmxResponseTO getAttributes(JmxQueryTO queryTo);

	List<MBeanOperationInfo> getOperations(String serverMappingKey, String domain, String mbean);

	List<JsTree> buildTree();

	JmxResponseTO getOpperation(JmxQueryTO queryTo);

	JmxResponseTO getExecuteOperation(JmxExecuteTO queryTo) throws Exception;

}
