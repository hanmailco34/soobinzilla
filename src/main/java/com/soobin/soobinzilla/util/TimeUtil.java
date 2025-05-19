package com.soobin.soobinzilla.util;

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoField;
import java.util.Date;

import lombok.experimental.UtilityClass;

@UtilityClass
public class TimeUtil {

	public static Long getNowMillisecond() {
		ZoneId zoneid = ZoneId.of("UTC");
		return LocalDateTime.now().atZone(zoneid).toInstant().toEpochMilli();
	}
	
	public static Boolean isExpire(Long expireTime) {
		return getNowMillisecond() > expireTime;
	}
	
	public static Boolean isSameDay(LocalDateTime dt1, LocalDateTime dt2) {
		return dt1.toLocalDate().equals(dt2.toLocalDate());
	}
	
	public static Boolean isSameWeek(LocalDateTime dt1, LocalDateTime dt2) {
		return dt1.get(ChronoField.ALIGNED_WEEK_OF_YEAR) == dt2.get(ChronoField.ALIGNED_WEEK_OF_YEAR)
				&& dt1.getYear() == dt2.getYear();
	}
	
	public static Boolean isSameMonth(LocalDateTime dt1, LocalDateTime dt2) {
		return dt1.getMonth() == dt2.getMonth() && dt1.getYear() == dt2.getYear();
	}
	
	public static Boolean validHour(Integer hour) {
		return hour != null && hour >= 0 && hour <= 23;
	}
	
	public static Boolean validMinute(Integer minute) {
		return minute != null && minute >= 0 && minute <= 59;
	}
	
	public static Boolean validDayOfWeek(Integer dayOfWeek) {
		return dayOfWeek != null && dayOfWeek >= 1 && dayOfWeek <= 7;
	}
	
	public static Boolean validDayOfMonth(Integer dayOfMonth) {
		return dayOfMonth != null && dayOfMonth >= 1 && dayOfMonth <= 31;
	}
	
	public static String toStringDate(Date date, String format) {
		SimpleDateFormat sdf = new SimpleDateFormat(format);
		return sdf.format(date);
	}
}
