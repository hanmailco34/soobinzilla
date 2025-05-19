package com.soobin.soobinzilla.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DepartmentInsertDto {
	@Schema(description = "부서 이름", example = "인사부")
	private String	name;
	@Schema(description = "부서 관리자 ID(user의 id컬럼)", example = "1")
	private Long	managerId;
	@Schema(description = "상위 부서 ID", example = "2")
	private Long	parentId;
}
