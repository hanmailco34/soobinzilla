package com.soobin.soobinzilla.filetransfer.core;

import static java.util.concurrent.TimeUnit.SECONDS;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import com.soobin.soobinzilla.util.ConstantUtil;

public class FileTransferThread extends ThreadPoolExecutor {
	private CountDownLatch countDownLatch;
    private final AtomicInteger atomicInteger;
    private Runnable callback;

	public FileTransferThread(int corePoolSize, int maximumPoolSize, long keepAliveTime, TimeUnit unit,
			BlockingQueue<Runnable> workQueue, RejectedExecutionHandler handler) {
		super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, handler);
		this.countDownLatch = new CountDownLatch(1);
		this.atomicInteger = new AtomicInteger(0);        
	}
	
	public static FileTransferThread getInstance(){
        return LazyHolder.INSTANCE;
    }
	
	private static class LazyHolder {
		private static final FileTransferThread INSTANCE = createInstance();
		private static FileTransferThread createInstance() {
			BlockingQueue<Runnable> queue = new LinkedBlockingQueue<>(ConstantUtil.THREAD_QUEUE_SIZE);
			return new FileTransferThread(ConstantUtil.THREAD_CORE_SIZE, ConstantUtil.THREAD_MAX_POOL_SIZE, ConstantUtil.THREAD_KEEP_ALIVE_TIME, SECONDS, queue,
					new ThreadPoolExecutor.CallerRunsPolicy());
		}
    }
    
    @Override
	protected void afterExecute(Runnable r, Throwable t) {
		super.afterExecute(r, t);
		if(this.atomicInteger.decrementAndGet() == 0) countDownLatch.countDown();
	}
    
    public void add(Runnable r) {
    	this.atomicInteger.incrementAndGet();
    	
		LazyHolder.INSTANCE.execute(r);
    }
	
	public int getQueueCount() {
        return LazyHolder.INSTANCE.getQueue().size();
    }

    public int getThreadActiveCount() {
        return LazyHolder.INSTANCE.getActiveCount();
    }
    
    public void setCallback(Runnable callback) {
    	this.callback = callback;
    }
    
    public void waitFileTransferThread() throws InterruptedException {
    	countDownLatch.await();
    	this.countDownLatch = new CountDownLatch(1);
    	if(this.callback != null) this.callback.run();
    }
}
