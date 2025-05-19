package com.soobin.soobinzilla.filetransfer.factory;

import com.jcraft.jsch.JSch;
import com.soobin.soobinzilla.filetransfer.core.FileTransferClientFactory;

public class SFTPClientFactory extends FileTransferClientFactory<JSch> {

	private SFTPClientFactory() {
		super(JSch::new);
	}
	
	private static class SingleTonHelper {
        private static final SFTPClientFactory INSTANCE = new SFTPClientFactory();
    }
	
	public static SFTPClientFactory getInstance() {
        return SingleTonHelper.INSTANCE;
    }
}
