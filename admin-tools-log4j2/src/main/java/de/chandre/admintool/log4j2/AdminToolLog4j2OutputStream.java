package de.chandre.admintool.log4j2;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;

/**
 * 
 * @author Andre
 *
 */
public class AdminToolLog4j2OutputStream extends OutputStream{
	private ByteArrayOutputStream buffer = new ByteArrayOutputStream();
	private Charset characterSet = Charset.forName("UTF-8");
	
	public AdminToolLog4j2OutputStream() {
	}
	
	public AdminToolLog4j2OutputStream(int size) {
		this.buffer = new ByteArrayOutputStream(size);
	}
	
	public AdminToolLog4j2OutputStream(int size, String characterSet) {
		this.buffer = new ByteArrayOutputStream(size);
		this.characterSet = Charset.forName(characterSet);
	}
	
	private synchronized ByteArrayOutputStream getBuffer() {
		return this.buffer;
	}
 
	@Override
	public void write(int b) throws IOException {
		getBuffer().write(b);
	}
 
	@Override
	public void write(byte[] b) throws IOException {
		getBuffer().write(b);
	}
 
	@Override
	public void write(byte[] b, int off, int len) throws IOException {
		getBuffer().write(b, off, len);
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
 
	public void setCharacterSet(Charset characterSet) {
		this.characterSet = characterSet;
	}
	
	public void setCharacterSet(String characterSet) {
		this.characterSet = Charset.forName(characterSet);
	}
	
	public void reset() {
		getBuffer().reset();
	}
	
	public String getAndReset() {
		String res = new String(getBuffer().toByteArray(), getCharacterSet());
		getBuffer().reset();
		return res;
	}
	
	public String getAndReset(String characterSet) {
		if (null == characterSet || characterSet.trim().isEmpty()) {
			return getAndReset();
		}
		String res = new String(getBuffer().toByteArray(), Charset.forName(characterSet));
		getBuffer().reset();
		return res;
	}
 
	@Override
	public String toString() {
		return new String(getBuffer().toByteArray(), getCharacterSet());
	}
}