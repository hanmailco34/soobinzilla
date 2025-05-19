package com.soobin.soobinzilla.model;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.soobin.soobinzilla.model.enums.ProtocolType;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = "zilla_connection_config")
public class ConnectionConfig extends BaseTime {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
	
	@Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private ProtocolType protocol;
	
	@Column(nullable = false)
	private String serverDirectory;
	
	@Column(nullable = false)
	private String localDirectory;
	
	@Column(nullable = false)
	@Builder.Default()
	private Boolean isThread = false;
	
	@Column(nullable = false)
	@Builder.Default()
	private Boolean isRecursive = true;
	
	@Column(nullable = false)
	@Builder.Default()
	private Boolean isDownload = true;
	
	@OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, mappedBy = "connectionConfig")
    private ProtocolConfig protocolConfig;
	
	public void update(ProtocolType protocol, String serverDirectory, String localDirectory, Boolean isThread, Boolean isRecursive, ProtocolConfig protocolConfig) {
		this.protocol = (protocol != null) ? protocol : this.protocol;
	    this.serverDirectory = (serverDirectory != null) ? serverDirectory : this.serverDirectory;
	    this.localDirectory = (localDirectory != null) ? localDirectory : this.localDirectory;
	    this.isThread = (isThread != null) ? isThread : this.isThread;
	    this.isRecursive = (isRecursive != null) ? isRecursive : this.isRecursive;
	    this.protocolConfig = (protocolConfig != null) ? protocolConfig : this.protocolConfig;
	}
	
	public void updateDirectory(String localDirectory, String serverDirectory) {
		this.localDirectory = localDirectory;
		this.serverDirectory = serverDirectory;
	}
}
