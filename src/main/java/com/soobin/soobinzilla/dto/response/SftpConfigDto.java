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
public class SftpConfigDto extends ProtocolConfigDto {
	@Schema(description = "호스트 키 사용 여부", example = "false")
	private Boolean hostKeyEnabled;
}
