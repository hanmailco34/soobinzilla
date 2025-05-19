package com.soobin.soobinzilla.dto.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DepartmentDeleteDto {
	@Schema(description = "부서 ID", example = "1")
	private Long	id;
	@Schema(description = "모든 하위 부서 삭제 여부", example = "true")
	private Boolean deleteAll;
	@Schema(description = "deleteAll이 false일 경우 새로운 상위 부서 ID로 교체", example = "2")
	private Long	newParentId;
	@Schema(description = "삭제되는 부서를 해당 부서로 교체", example = "2")
	private Long	updateDepartmentId;
}
