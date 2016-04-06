package de.chandre.admintool.db;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * @author Andre
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.PROPERTY)
public class ExampleStatements implements Serializable
{
	private static final long serialVersionUID = 1L;
	
	private String datasourceName;
	private Map<String, List<ExampleStatement>> clusters = new LinkedHashMap<>();

	/**
	 * @return the datasourceName
	 */
	public String getDatasourceName() {
		return datasourceName;
	}

	/**
	 * @param datasourceName the datasourceName to set
	 */
	public void setDatasourceName(String datasourceName) {
		this.datasourceName = datasourceName;
	}

	public Map<String, List<ExampleStatement>> getClusters() {
		return clusters;
	}
	
	public void addClusters(String clusterName, List<ExampleStatement> examples) {
		this.clusters.put(clusterName, examples);
	}
	
	public void addExample(String clusterName, ExampleStatement examples) {
		if (null == this.clusters.get(clusterName)) {
			this.clusters.put(clusterName, new ArrayList<>());
		}
		this.clusters.get(clusterName).add(examples);
	}

	public void setClusters(Map<String, List<ExampleStatement>> clusters) {
		this.clusters = clusters;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ExampleStatements [datasourceName=").append(datasourceName).append(", clusters=")
				.append(clusters).append("]");
		return builder.toString();
	}
}
