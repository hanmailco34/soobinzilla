package com.soobin.soobinzilla.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.apache.commons.net.ftp.FTP;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "zilla_fpt_config")
public class FtpConfig extends ProtocolConfig {
	@Column(nullable = false)
	@Builder.Default()
    private String encoding = "UTF-8";
    
    @Column(nullable = false)
    @Builder.Default()
    private Integer fileType = FTP.BINARY_FILE_TYPE;
    
    @Column(nullable = false)
    @Builder.Default()
    private Integer fileTransferMode = FTP.BINARY_FILE_TYPE;
    
    @Column(nullable = false)
    @Builder.Default()
    private Boolean autoDetectUtf8 = Boolean.TRUE;
    
    @Column(nullable = false)
    @Builder.Default()
    private Boolean passiveMode = Boolean.FALSE;
    
    @Column(nullable = false)
    @Builder.Default()
    private Integer connectTime = 5000;
    
    @Column(nullable = false)
    @Builder.Default()
    private Integer dataTime = 10000;
    
    public void update(String encoding, Integer fileType, Integer fileTransferMode, Boolean autoDetectUtf8, Boolean passiveMode, Integer connectTime, Integer dataTime) {
    	this.encoding = (encoding != null) ? encoding : this.encoding;
        this.fileType = (fileType != null) ? fileType : this.fileType;
        this.fileTransferMode = (fileTransferMode != null) ? fileTransferMode : this.fileTransferMode;
        this.autoDetectUtf8 = (autoDetectUtf8 != null) ? autoDetectUtf8 : this.autoDetectUtf8;
        this.passiveMode = (passiveMode != null) ? passiveMode : this.passiveMode;
        this.connectTime = (connectTime != null) ? connectTime : this.connectTime;
        this.dataTime = (dataTime != null) ? dataTime : this.dataTime;
    }
}
