package com.colingodsey.logos.utils

import java.util
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{AbstractExecutorService, ExecutorService, ThreadFactory}
import javafx.application.Platform

import scala.concurrent.duration._
/*
class JfxExecutor(config: Config,
		prerequisites: DispatcherPrerequisites)
		extends ExecutorServiceConfigurator(config, prerequisites) {
  val maxQueued = 100000

  private val queuedCounter = new AtomicInteger(0)

	def createExecutorServiceFactory(id: String,
			threadFactory: ThreadFactory): ExecutorServiceFactory = new ExecutorServiceFactory {
		def createExecutorService: ExecutorService = new AbstractExecutorService {
			//Logging(LogSource.fromAnyClass[this.type])
			val clz = JfxExecutor.this.getClass
			val logging = new BusLogging(prerequisites.eventStream, id, clz)

			logging.info("JfxExecutor started")

			def shutdown(): Unit = {}

			def shutdownNow(): util.List[Runnable] = new util.Vector

			def isShutdown: Boolean = Platform.isImplicitExit // ??

			def isTerminated: Boolean = Platform.isImplicitExit // ??

			def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = true

			def execute(command: Runnable): Unit = {
        if(queuedCounter.get() > maxQueued) {
          logging.debug("dropping " + command)
        } else {
          queuedCounter.incrementAndGet()
          Platform.runLater(new Runnable {
            lazy val run = try {
              queuedCounter.decrementAndGet()
              command.run()
            } catch { case t: Throwable =>
              logging.error(t, "Uncaught exception!")
            }
          })
        }
      }
		}
	}
}*/