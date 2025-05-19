package com.soobin.soobinzilla.util;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.exception.FileTransferException;

import lombok.experimental.UtilityClass;

@UtilityClass
public class PageUtil {
	public static Pageable getPageable(PageDto dto) throws FileTransferException {
		ValidUtil.checkPage(dto);
		String sortBy = Boolean.TRUE.equals(ObjectUtil.isEmpty(dto.getSortBy())) ? "id" : dto.getSortBy();
		Sort sort = dto.getOrder().equalsIgnoreCase(Sort.Direction.ASC.name()) 
				? Sort.by(sortBy).ascending()
				: Sort.by(sortBy).descending();
		return PageRequest.of(dto.getPage() -1, dto.getPageSize(), sort);
	}
	
}
