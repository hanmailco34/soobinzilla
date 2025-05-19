package com.soobin.soobinzilla.exception.handler;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.SecurityErrorCode;
import com.soobin.soobinzilla.response.ResponseSecurityExceptionHandler;
import com.soobin.soobinzilla.util.LogUtil;

@Component
public class SecurityAccessDeniedHandler implements AccessDeniedHandler {

	@Override
	public void handle(HttpServletRequest request, HttpServletResponse response,
			AccessDeniedException accessDeniedException) throws IOException, ServletException {
		try {
			ResponseSecurityExceptionHandler.error(response, new FileTransferException(SecurityErrorCode.SECURITY_FORBIDDEN));
		} catch (FileTransferException e) {
			LogUtil.error(e);
		}
	}

}
