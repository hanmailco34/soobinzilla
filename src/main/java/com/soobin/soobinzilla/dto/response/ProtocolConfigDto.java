package com.soobin.soobinzilla.dto.response;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.soobin.soobinzilla.model.enums.ProtocolType;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    include = JsonTypeInfo.As.PROPERTY,
    property = "type"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = FtpConfigDto.class, name = "FTP"),
    @JsonSubTypes.Type(value = SftpConfigDto.class, name = "SFTP"),
    @JsonSubTypes.Type(value = SmbConfigDto.class, name = "SMB"),
})
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public abstract class ProtocolConfigDto {
	@Schema(description = "프로토콜 설정 ID", example = "1")
	private Long	id;
	@Schema(description = "프로토콜 타입", example = "FTP")
	private ProtocolType type;
}
