package de.chandre.admintool;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "at.example", ignoreUnknownFields=true)
@ManagedResource(objectName = "de.chandre.admintool:type=Config,name=Example2Configuration", description = "Configuration Example for JMX Browser")
public class ExampleMXBean2 {
	
	private String simpleString;
	
	private Map<String, String> simpleMap = new HashMap<>();
	
	private List<String> simpleList = new ArrayList<>();

	@ManagedAttribute(description="Description for Simple String getter")
	public String getSimpleString() {
		return simpleString;
	}

	@ManagedAttribute(description="Description for Simple String getter")
	public void setSimpleString(String simpleString) {
		this.simpleString = simpleString;
	}

	@ManagedAttribute(description="Description for Simple Map getter")
	public Map<String, String> getSimpleMap() {
		return simpleMap;
	}

	@ManagedOperation(description="Description for Simple Map setter")
	public void setSimpleMap(Map<String, String> simpleMap) {
		this.simpleMap = simpleMap;
	}

	@ManagedAttribute(description="Description for Simple List getter")
	public List<String> getSimpleList() {
		return simpleList;
	}

	@ManagedOperation(description="Description for Simple List setter")
	public void setSimpleList(List<String> simpleList) {
		this.simpleList = simpleList;
	}
	
	@ManagedOperation(description="Description for Simple execution")
	public void operationWithNoArg() {
		System.out.println("operationWithNoArg called");
	}
	
	@ManagedOperation(description="Description for Simple arg setter")
	public void operationWithArg(String arg) {
		System.out.println("operationWithArg called: " + arg);
	}
	
	@ManagedOperation(description="Description for Simple arg getter")
	public String operationWithNoArgResp() {
		System.out.println("operationWithNoArg called");
		return "Hello World";
	}
	
	@ManagedOperation(description="Description for Simple arg setter")
	public String operationWithArgResp(String arg) {
		System.out.println("operationWithArg called: " + arg);
		return String.format("Hello World Arg (arg: %s) ", arg);
	}
	
	@ManagedOperation(description="Description for multiple args setter")
	public String operationWithArgResp2(String arg, String myOtherArg) {
		System.out.println("operationWithArg called: " + arg + ", " + myOtherArg);
		return String.format("Hello World Arg 2 (arg: %s, myOtherArg: %s) ", arg, myOtherArg);
	}
	
	@ManagedOperation(description="Description for Simple List getter")
	public List<MyObject> operationWithNoArgListResp() {
		System.out.println("operationWithNoArg called");
		
		List<MyObject> result = new ArrayList<>();
		result.add(new MyObject("Object 1", 1));
		
		MyObject o2 = new MyObject("Object 2", 2);
		o2.addProp("Property 1", "Value 1");
		o2.addProp("Property 2", "Value 2");
		result.add(o2);
		
		result.add(new MyObject("Object 3", 54656));

		return result;
	}
	
	private static class MyObject {
		String name;
		int value;
		Map<String, String> props;

		public MyObject(String name, int value) {
			super();
			this.name = name;
			this.value = value;
		}
		
		public void addProp(String name, String value) {
			if(this.props == null) {
				this.props = new HashMap<>(2);
			}
			this.props.put(name, value);
		}
	}
	
}
