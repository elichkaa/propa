package producerconsumer;

public class ProducerConsumer {

	public static void main(String[] args) {
		
		Buffer buffer = new Buffer();

		//Start two producer-threads and one consumer-thread here.
		//All producer-threads and consumer-threads have to use the buffer instance "buffer".
		//The producer and consumer threads have to call produce/consume of the buffer until
		//stopRunning() of the buffer is called (boolean "running" in Buffer is set to "false")
		
		try {
			Thread.sleep(10000);
		} catch (InterruptedException e1) {
			buffer.stopRunning();
		} finally {
			buffer.stopRunning();
		}

		//Wait for the end of Producer- and Consumer-Thread execution. 
		//Don't forget to end eventually sleeping and waiting threads.
	}
}
