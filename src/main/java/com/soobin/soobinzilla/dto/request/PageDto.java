package com.soobin.soobinzilla.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PageDto {
	@Schema(description = "현재 페이지 번호", example = "1")
	private Integer	page;
	@Schema(description = "한 페이지에 표시할 항목 수", example = "1")
	private Integer	pageSize;
	@Schema(description = "정렬 기준", example = "id")
	private String	sortBy;
	@Schema(description = "정렬 순서(asc, desc)", example = "desc")
	private String	order;
	@Schema(description = "검색 기준", example = "")
	private String	searchBy;
	@Schema(description = "검색어", example = "")
	private String	search;	
}
