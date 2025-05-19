package com.soobin.soobinzilla.exception.handler;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.SecurityErrorCode;
import com.soobin.soobinzilla.response.ResponseSecurityExceptionHandler;
import com.soobin.soobinzilla.util.LogUtil;

@Component
public class SecurityAuthenticationHandler implements AuthenticationEntryPoint {

	@Override
	public void commence(HttpServletRequest request, HttpServletResponse response,
			AuthenticationException authException) throws IOException, ServletException {
		try {
			ResponseSecurityExceptionHandler.error(response, new FileTransferException(SecurityErrorCode.SECURITY_UNAUTHORIZED));
		} catch (FileTransferException e) {
			LogUtil.error(e);
		}
	}

}
