package de.chandre.admintool.security.dbuser;

import java.time.ZonedDateTime;

import de.chandre.admintool.security.dbuser.domain.User;

/**
 * some static utils
 * 
 * @author Andre
 *
 */
public class ATSecDBUtils {
	
	private ATSecDBUtils() {
	}
	
	/**
	 * returns the current time of users time zone
	 * @param user
	 * @return
	 */
	public static ZonedDateTime getNowZoned(User user) {
		ZonedDateTime time = null;
		if (null != user.getTimeZone()) {
			try {
				time = ZonedDateTime.now(user.getTimeZoneAsTimeZone().toZoneId());
			} catch (Exception e) {
				
			}
		}
		if(null == time) {
			time = ZonedDateTime.now();
		}
		return time;
	}

}
