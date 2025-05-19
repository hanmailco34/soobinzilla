package com.soobin.soobinzilla.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.soobin.soobinzilla.util.ConstantUtil;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "zilla_token")
public class AuthToken extends BaseTime {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long	id;
	
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name= "user_id", referencedColumnName = "id", nullable = false)
	private User user;
	
	@Column(nullable = false, length = ConstantUtil.TOKEN_LENGTH)
	private String accessToken;
	
	@Column(nullable = false, length = ConstantUtil.TOKEN_LENGTH)
	private String refreshToken;
	
	@Column(length = 50)
	private String ipAddress;
	
	@Column(nullable = false)
	@Builder.Default()
	private Boolean isActive = true;
	
	public void updateAccessToken(String accessToken) {
		this.accessToken = accessToken;
	}
}
