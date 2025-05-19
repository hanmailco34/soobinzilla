package com.soobin.soobinzilla.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "zilla_sfpt_config")
public class SftpConfig extends ProtocolConfig {
	@Column(nullable = false)
	@Builder.Default()
	private Boolean hostKeyEnabled = Boolean.FALSE;
	
	public void update(Boolean hostKeyEnabled) {
		this.hostKeyEnabled = (hostKeyEnabled != null) ? hostKeyEnabled : this.hostKeyEnabled;
	}
}
