package com.soobin.soobinzilla.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DepartmentDto {
	private Long	id;
	private String	name;
	private Long	managerId;
	private Long	parentId;
}
