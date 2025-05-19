package com.soobin.soobinzilla.response;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.MediaType;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.LogUtil;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ResponseSecurityExceptionHandler {

	public static void error(HttpServletResponse response, FileTransferException exception) throws FileTransferException {
		LogUtil.error(exception);
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.setStatus(exception.getError().getCode());
        
        Map<String, String> result = new HashMap<>();
        result.put("status", ConstantUtil.STATUS_OOPS);
        result.put("message", exception.getError().getDescription());
        
        try (OutputStream os = response.getOutputStream()) {
            ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.writeValue(os, result);
            os.flush();
        } catch (IOException e) {
			throw new FileTransferException(e);
		}
	}
}
