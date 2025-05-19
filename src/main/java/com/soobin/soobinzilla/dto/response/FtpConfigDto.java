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
public class FtpConfigDto extends ProtocolConfigDto {
	@Schema(description = "파일 인코딩", example = "UTF-8")
    private String 	encoding;
	@Schema(description = "파일 타입", example = "2(BINARY)")
    private Integer fileType;
	@Schema(description = "파일 전송 모드", example = "2(BINARY)")
    private Integer fileTransferMode;
	@Schema(description = "UTF-8 자동 감지 여부", example = "true")
    private Boolean autoDetectUtf8;
	@Schema(description = "패시브 모드 여부", example = "false")
    private Boolean passiveMode;
	@Schema(description = "연결 시간 (밀리초)", example = "5000")
    private Integer connectTime;
	@Schema(description = "데이터 전송 시간 (밀리초)", example = "10000")
    private Integer dataTime;
}
