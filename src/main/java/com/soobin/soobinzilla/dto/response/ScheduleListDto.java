package com.soobin.soobinzilla.dto.response;

import com.soobin.soobinzilla.model.enums.ScheduleType;

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
public class ScheduleListDto {
	@Schema(description = "스케줄 ID", example = "1")
	private Long			id;
	@Schema(description = "스케줄 타입", example = "DAILY")
	private ScheduleType	type;
	@Schema(description = "간격 (분)", example = "30")
	private Integer			intervalMinutes;
	@Schema(description = "시 (0~23)", example = "14")
	private	Integer			hour;
	@Schema(description = "분 (0~59)", example = "30")
	private Integer			minute;
	@Schema(description = "요일 (1~7, 1=월요일)", example = "2")
	private	Integer			dayOfWeek;
	@Schema(description = "일 (1~31)", example = "15")
	private Integer			dayOfMonth;
	@Schema(description = "활성 상태 여부", example = "true")
	private	Boolean			isActive;
	@Schema(description = "연결 정보")
	private ConnectionDto	connectionDto;
}
