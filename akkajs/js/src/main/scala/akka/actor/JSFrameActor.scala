package akka.actor

import akka.actor.internal.Cell
import org.scalajs.{dom => window}

object JSFrameActor {
  case object AnimationFrame
}

trait JSFrameActor extends Actor {
  private var animationFrameRequested = false

  checkFrameRequest()

  private def checkFrameRequest(): Unit = if(!animationFrameRequested) {
    animationFrameRequested = true

    //this lambda can only be called from a fresh stack, so we're sure we arent re-entering anything
    window.requestAnimationFrame { _: Double =>
      animationFrameRequested = false

      if(context.asInstanceOf[Cell].isRunning) {
        internalReceive(JSFrameActor.AnimationFrame)
        checkFrameRequest()
      }
    }
  }
}
