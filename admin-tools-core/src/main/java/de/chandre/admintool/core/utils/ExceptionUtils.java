package de.chandre.admintool.core.utils;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.PrintStream;

/**
 * 
 * @author Andre
 *
 */
public class ExceptionUtils {
	
	private ExceptionUtils() {
	}

	/**
	 * prints a exception into string
	 * @param throwable
	 * @return
	 */
	public static String printException(final Throwable throwable) {
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

	private static void closeStream(final Closeable closeable) {
		if (null != closeable) {
			try {
				closeable.close();
			} catch (Exception ignore) {
			}
		}
	}
}
