package com.soobin.soobinzilla.dto.request;

import java.util.List;

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
public class PermissionInsertDto {
	private Long	id;
	private Long	userId;
	private Long	scheduleId;
	private List<String>	path;
}
