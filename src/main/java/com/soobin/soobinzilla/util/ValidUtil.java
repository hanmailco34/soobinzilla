package com.soobin.soobinzilla.util;

import java.util.regex.Pattern;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.DBErrorCode;

import lombok.experimental.UtilityClass;

@UtilityClass
public class ValidUtil {

	public static Boolean isLengthOver(String str, Integer length) {
		return str.length() > length;
	}
	
	public static Boolean isPattern(String str, String reg) {
		Pattern pattern = Pattern.compile(reg);
		return pattern.matcher(str).matches();
	}
	
	public static void checkNull(Object... values) throws FileTransferException {
		for(Object value : values) {
			if(Boolean.TRUE.equals(ObjectUtil.isEmpty(value))) throw new FileTransferException(DBErrorCode.NOT_NULL);
		}
	}
	
	public static void checkPage(PageDto dto) throws FileTransferException {
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(dto.getPage())) || Boolean.TRUE.equals(ObjectUtil.isEmpty(dto.getPageSize())) || dto.getPage() <= 0 || dto.getPageSize() <= 0) throw new FileTransferException(DBErrorCode.NOT_PAGE);
		if(!(Boolean.TRUE.equals(ObjectUtil.isEmpty(dto.getOrder())) || ConstantUtil.ASC.equalsIgnoreCase(dto.getOrder()) || ConstantUtil.DESC.equalsIgnoreCase(dto.getOrder()))) throw new FileTransferException(DBErrorCode.NOT_ORDER);
	}
}
