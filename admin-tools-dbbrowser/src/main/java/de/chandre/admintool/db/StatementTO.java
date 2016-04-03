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

	private int tab;
	private String datasourceName;
	
	private String clobEncoding;
	private boolean showClobs;
	private boolean showBlobs;
	
	private int maxResults;
	
	private String statement;

	
	/**
	 * @return the tab
	 */
	public int getTab() {
		return tab;
	}

	/**
	 * @param tab the tab to set
	 */
	public void setTab(int tab) {
		this.tab = tab;
	}

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
	 * @return the showClobs
	 */
	public boolean isShowClobs() {
		return showClobs;
	}

	/**
	 * @param showClobs the showClobs to set
	 */
	public void setShowClobs(boolean showClobs) {
		this.showClobs = showClobs;
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
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("StatementTO [tab=").append(tab).append(", datasourceName=").append(datasourceName)
				.append(", clobEncoding=").append(clobEncoding).append(", showClobs=").append(showClobs)
				.append(", showBlobs=").append(showBlobs).append(", maxResults=").append(maxResults)
				.append(", statement=").append(statement).append("]");
		return builder.toString();
	}
	
}
