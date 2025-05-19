package com.soobin.soobinzilla.dto.response;

import com.soobin.soobinzilla.model.enums.ProtocolType;

import io.swagger.v3.oas.annotations.media.Schema;
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
public class ConnectionConfigDto {
	@Schema(description = "설정 ID", example = "1")
	private Long	id;
	@Schema(description = "재귀적 여부", example = "true")
	private	Boolean	isRecursive;
	@Schema(description = "스레드 사용 여부", example = "false")
	private Boolean	isThread;
	@Schema(description = "다운로드 여부(업로드할 대상일 경우 false)", example = "true")
	private Boolean isDownload;
	@Schema(description = "로컬 디렉토리 경로", example = "/local/path")
	private String	localDirectory;
	@Schema(description = "서버 디렉토리 경로", example = "/server/path")
	private String	serverDirectory;
	@Schema(description = "프로토콜 종류", example = "FTP")
	private ProtocolType protocol;
	@Schema(description = "프로토콜 설정", implementation = ProtocolConfigDto.class)
	private ProtocolConfigDto protocolConfig;
}
