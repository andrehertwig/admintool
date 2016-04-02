package de.chandre.admintool.db;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * AdminTool dbbrowser executable statement transfer object
 * @author Andre
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.PROPERTY)
public class StatementTO implements Serializable
{
	private static final long serialVersionUID = 1L;

	private String datasourceName;
	
	private String clobEncoding;
	private boolean showBlobs;
	
	private int maxResults;
	
	private String statement;

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

	/**
	 * @return the clobEncoding
	 */
	public String getClobEncoding() {
		return clobEncoding;
	}

	/**
	 * @param clobEncoding the clobEncoding to set
	 */
	public void setClobEncoding(String clobEncoding) {
		this.clobEncoding = clobEncoding;
	}

	/**
	 * @return the showBlobs
	 */
	public boolean isShowBlobs() {
		return showBlobs;
	}

	/**
	 * @param showBlobs the showBlobs to set
	 */
	public void setShowBlobs(boolean showBlobs) {
		this.showBlobs = showBlobs;
	}

	/**
	 * @return the maxResults
	 */
	public int getMaxResults() {
		return maxResults;
	}

	/**
	 * @param maxResults the maxResults to set
	 */
	public void setMaxResults(int maxResults) {
		this.maxResults = maxResults;
	}

	/**
	 * @return the statement
	 */
	public String getStatement() {
		return statement;
	}

	/**
	 * @param statement the statement to set
	 */
	public void setStatement(String statement) {
		this.statement = statement;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("StatementTO [datasourceName=").append(datasourceName).append(", clobEncoding=")
				.append(clobEncoding).append(", showBlobs=").append(showBlobs).append(", maxResults=")
				.append(maxResults).append(", statement=").append(statement).append("]");
		return builder.toString();
	}
	
}
