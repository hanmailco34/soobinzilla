package com.soobin.soobinzilla.util;

import javax.servlet.http.HttpServletRequest;

public class IPUtil {

	private static final String[] IP_HEADER_CANDIDATES = {
            "X-Forwarded-For",
            "Proxy-Client-IP",
            "WL-Proxy-Client-IP",
            "HTTP_CLIENT_IP",
            "HTTP_X_FORWARDED_FOR",
            "X-Real-IP",
            "X-RealIP",
            "REMOTE_ADDR"
    };
	
	public static String getClientIp(HttpServletRequest request) {
		if (request == null) {
            return "unknown";
        }
		String ip;
		for (String header : IP_HEADER_CANDIDATES) {
            ip = request.getHeader(header);
            if (isValidIp(ip)) {
                return ip.split(",")[0];
            }
        }
		
		ip = request.getRemoteAddr();
		
		return ip;
	}
	
	private static boolean isValidIp(String ip) {
        return ip != null && ip.length() > 0 && !"unknown".equalsIgnoreCase(ip);
    }
}
