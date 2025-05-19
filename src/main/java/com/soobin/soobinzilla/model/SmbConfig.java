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
@Table(name = "zilla_smb_config")
public class SmbConfig extends ProtocolConfig {	
	@Column(nullable = false)
	@Builder.Default()
	private String	domain = "";
	
	@Column(nullable = false)
	private String	section;
	
	public void update(String domain, String section) {
		this.domain = (domain != null) ? domain : this.domain;
		this.section = (section != null) ? section : this.section;
	}
}
