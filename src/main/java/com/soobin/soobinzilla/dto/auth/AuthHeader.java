package com.soobin.soobinzilla.dto.auth;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AuthHeader {	
	private Long	expireTime;
	private String	tokenType;
	private Integer	keyIndex;
}
