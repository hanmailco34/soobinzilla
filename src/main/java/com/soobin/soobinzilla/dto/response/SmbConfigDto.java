package com.soobin.soobinzilla.dto.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class SmbConfigDto extends ProtocolConfigDto {
	@Schema(description = "도메인", example = "WORKGROUP")
	private String	domain;
	@Schema(description = "섹션", example = "sharedFolder")
	private String	section;
}
