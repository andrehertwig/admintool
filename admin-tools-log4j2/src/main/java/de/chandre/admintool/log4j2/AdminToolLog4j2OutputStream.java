package de.chandre.admintool.log4j2;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import org.apache.logging.log4j.Level;

/**
 * OutputStream wrapper for Logging.
 * @author Andre
 * @since 1.1.1
 */
public class AdminToolLog4j2OutputStream extends OutputStream {
	private ByteArrayOutputStream buffer = new ByteArrayOutputStream();
	private Charset characterSet = Charset.forName("UTF-8");
	
	private Map<String, Level> originalLevels = new HashMap<>();
	
	/**
	 * OutputStream wrapper with ByteArrayOutputStream with size = 4096 and characterSet = UTF-8
	 */
	public AdminToolLog4j2OutputStream() {
		this.buffer = new ByteArrayOutputStream(4096);
	}
	
	/**
	 * OutputStream wrapper with ByteArrayOutputStream with size and characterSet = UTF-8
	 * @param size
	 */
	public AdminToolLog4j2OutputStream(int size) {
		this.buffer = new ByteArrayOutputStream(size);
	}
	
	/**
	 * OutputStream wrapper with ByteArrayOutputStream with size and characterSet
	 * @param size
	 * @param characterSet
	 */
	public AdminToolLog4j2OutputStream(int size, String characterSet) {
		this.buffer = new ByteArrayOutputStream(size);
		this.characterSet = Charset.forName(characterSet);
	}
 
	@Override
	public void write(int b) throws IOException {
		synchronized (buffer) {
			this.buffer.write(b);
		}
	}
 
	@Override
	public void write(byte[] b) throws IOException {
		synchronized (buffer) {
			this.buffer.write(b);
		}
	}
 
	@Override
	public void write(byte[] b, int off, int len) throws IOException {
		synchronized (buffer) {
			this.buffer.write(b, off, len);
		}
	}
	
	@Override
	public void close() throws IOException {
		this.buffer.close();
		super.close();
	}
 
	/**
	 * Get currently set character set if none set default charset of UTF-8 will be used
	 * @return Charaset 
	 */
	public Charset getCharacterSet() {
		return characterSet;
	}
 
	/**
	 * change the character set
	 * @param characterSet
	 */
	public void setCharacterSet(Charset characterSet) {
		this.characterSet = characterSet;
	}
	
	/**
	 * change the character set
	 * @param characterSet
	 */
	public void setCharacterSet(String characterSet) {
		this.characterSet = Charset.forName(characterSet);
	}
	
	/**
	 * just resets the buffer (ByteArrayOutputStream)
	 */
	public void reset() {
		this.buffer.reset();
	}
	
	/**
	 * returns the content of the buffer (ByteArrayOutputStream) and resets it
	 * @return
	 */
	public String getAndReset() {
		synchronized (buffer) {
			String res = new String(this.buffer.toByteArray(), getCharacterSet());
			this.buffer.reset();
			return res;
		}
	}
	
	/**
	 * returns the content of the buffer (ByteArrayOutputStream) with specific characterSet and resets it
	 * @param characterSet
	 * @return
	 */
	public String getAndReset(String characterSet) {
		if (null == characterSet || characterSet.trim().isEmpty()) {
			return getAndReset();
		}
		synchronized (buffer) {
			String res = new String(this.buffer.toByteArray(), Charset.forName(characterSet));
			this.buffer.reset();
			return res;
		}
	}
	
	public void addOriginalLevel(String loggerName, Level level) {
		this.originalLevels.put(loggerName, level);
	}
	
	public Level getOriginalLevel(String loggerName) {
		return this.originalLevels.get(loggerName);
	}
	
	public void clearOriginalLevels() {
		this.originalLevels.clear();
	}

	@Override
	public String toString() {
		return new String(this.buffer.toByteArray(), getCharacterSet());
	}
}