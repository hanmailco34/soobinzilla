package com.soobin.soobinzilla.filetransfer.factory;

import com.hierynomus.smbj.SMBClient;
import com.hierynomus.smbj.SmbConfig;
import com.soobin.soobinzilla.filetransfer.core.FileTransferClientFactory;

public class SMBClientFactory extends FileTransferClientFactory<SMBClient> {

	private SMBClientFactory() {
		super(SMBClient::new);
	}
	
	private SMBClientFactory(SmbConfig config) {
		super(() -> new SMBClient(config));
	}
	
	private static class SingleTonHelper {
        private static final SMBClientFactory INSTANCE = new SMBClientFactory();
    }
	
	public static SMBClientFactory getInstance() {
        return SingleTonHelper.INSTANCE;
    }
	
	public static SMBClientFactory getInstance(SmbConfig config) { 
		return new SMBClientFactory(config);
	}
}
