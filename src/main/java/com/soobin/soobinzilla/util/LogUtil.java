package com.soobin.soobinzilla.util;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;

import com.soobin.soobinzilla.exception.FileTransferException;

import lombok.extern.log4j.Log4j2;

@Log4j2
public class LogUtil {
	
	private static final Logger connectionLog = LogManager.getLogger("connection");
	
	public static void connection(Long connectionId, String msg) {
		ThreadContext.put("connectionId", connectionId.toString());
		connectionLog.info("[Connection {}] MSG : {}", connectionId, msg);
		ThreadContext.remove("connectionId");
	}
	
	public static void error(FileTransferException exception) {
		error(exception, exception.getCode());
    }
	
	private static void error(Exception exception, int code) {
		StackTraceElement errorLocation = getErrorLocation(exception.getStackTrace());
		log.error(String.format("에러 발생 발생!! %s(CODE:%d)", exception.getMessage(), code));
		log.error(String.format("에러 발생 위치!! 클래스 %s, 메서드 %s, 라인 %d", errorLocation.getClassName(), errorLocation.getMethodName(), errorLocation.getLineNumber()));
    }

	private static StackTraceElement getErrorLocation(StackTraceElement[] stackTrace) {
		if (stackTrace.length > 0) {
            return stackTrace[0];
        }
		return null;
	}
}
