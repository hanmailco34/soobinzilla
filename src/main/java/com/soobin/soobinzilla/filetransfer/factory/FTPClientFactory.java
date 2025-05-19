package com.soobin.soobinzilla.filetransfer.factory;

import org.apache.commons.net.ftp.FTPClient;

import com.soobin.soobinzilla.filetransfer.core.FileTransferClientFactory;

public class FTPClientFactory extends FileTransferClientFactory<FTPClient> {

	private FTPClientFactory() {
		super(FTPClient::new);
	}
	
	private static class SingleTonHelper {
        private static final FTPClientFactory INSTANCE = new FTPClientFactory();
    }
	
	public static FTPClientFactory getInstance() {
        return SingleTonHelper.INSTANCE;
    }
}
