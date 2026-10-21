// Copyright (c) 2011-2025 ScalaMock Contributors (https://github.com/ScalaMock/ScalaMock/graphs/contributors)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

// placed in org.scalamock package to access MockContext
package org.scalamock.ziotest.internal

import org.scalamock.context.{Call, CallLog, MockContext}
import org.scalamock.handlers.{Handlers, OrderedHandlers, UnorderedHandlers}
import org.scalamock.ziotest.ExpectationException
import zio.{IO, UIO, ZIO}

import scala.collection.mutable.ListBuffer

/**
 * Creates mocks for zio-test. Verifies mock calls.
 *
 * You should never create ZIOMockContext manually (and there's no way to do so).
 * It will be created by library code for the user.
 */
sealed trait ZIOMockContext {
  // must be public for RunMockedMacros
  val mockContext: MockContext
  private[ziotest] def initializeExpectations(): UIO[Unit]
  private[ziotest] def verifyExpectations(): IO[ExpectationException, Unit]
  protected[ziotest] def inSequence[T](what: => T): T
  protected[ziotest] def inAnyOrder[T](what: => T): T
}

object ZIOMockContext {

  private[ziotest] class Live extends ZIOMockContext with MockContext {

    val mockContext: MockContext = this

    // Production code can hide an unexpected call:
    //
    //   // test
    //   (service.call _).expects(*).never()
    //   // production
    //   service.call(42).catchAllCause(_ => ZIO.unit)
    //
    // The same happens when a forked fiber fails and that failure is ignored.
    // Remember the call so verifyExpectations fails the test anyway.
    private val unexpectedCalls = new ListBuffer[Call]

    override type ExpectationException = org.scalamock.ziotest.ExpectationException

    override protected def newExpectationException(message: String, methodName: Option[Symbol]) = {
      // Add two line separators to improve error message display in console.
      // Without separators (need to scroll right to see):
      // Exception in thread "zio-fiber-69" org.scalamock.ziotest.ExpectationException: Unexpected call: <mock-2> YqlClient.groupDataAndInsert(Some(2025-02-04), yt-path)
      //
      // With separators (no scrolling needed):
      // Exception in thread "zio-fiber-69" org.scalamock.ziotest.ExpectationException:
      //
      //    Unexpected call: <mock-2> YqlClient.groupDataAndInsert(Some(2025-02-04), yt-path)
      val sep = System.lineSeparator()
      new ExpectationException(s"$sep$sep$message")
    }

    override private[scalamock] def reportUnexpectedCall(call: Call) = {
      recordUnexpectedCall(call)
      super.reportUnexpectedCall(call)
    }

    // copy-paste from org.scalamock.MockFactoryBase
    def initializeExpectations(): UIO[Unit] = ZIO.succeed {
      val initialHandlers = new UnorderedHandlers
      callLog = new CallLog

      expectationContext = initialHandlers
      currentExpectationContext = initialHandlers
      clearUnexpectedCalls()
    }

    private def clearExpectations(): UIO[Unit] = ZIO.succeed {
      // to forbid setting expectations after verification is done
      callLog = null
      expectationContext = null
      currentExpectationContext = null
    }

    def verifyExpectations(): IO[ExpectationException, Unit] = {
      for {
        _ <- ZIO.succeed(callLog.foreach { call =>
          val _ = expectationContext.verify(call)
        })
        oldCallLog = callLog
        oldExpectationContext = expectationContext
        oldUnexpectedCalls = drainUnexpectedCalls()
        _ <- clearExpectations()
        _ <- ZIO.when(oldUnexpectedCalls.nonEmpty || !oldExpectationContext.isSatisfied) {
          ZIO.fail(verificationFailure(oldUnexpectedCalls, oldCallLog, oldExpectationContext))
        }
      } yield ()

    }

    private def recordUnexpectedCall(call: Call): Unit =
      unexpectedCalls.synchronized {
        unexpectedCalls += call
      }

    private def clearUnexpectedCalls(): Unit =
      unexpectedCalls.synchronized {
        unexpectedCalls.clear()
      }

    private def drainUnexpectedCalls(): List[Call] =
      unexpectedCalls.synchronized {
        val calls = unexpectedCalls.toList
        unexpectedCalls.clear()
        calls
      }

    private def verificationFailure(
        unexpected: List[Call],
        oldCallLog: CallLog,
        oldExpectationContext: Handlers
    ): ExpectationException = {
      val context = errorContext(oldCallLog, oldExpectationContext)
      val message =
        if (unexpected.isEmpty) s"Unsatisfied expectation:\n\n$context"
        else {
          val calls = unexpected.map(call => s"Unexpected call: $call").mkString("\n")
          if (oldExpectationContext.isSatisfied) s"$calls\n\n$context"
          else s"$calls\n\nUnsatisfied expectation:\n\n$context"
        }
      newExpectationException(message)
    }

    override protected[ziotest] def inSequence[T](what: => T): T = {
      inContext(new OrderedHandlers)(what)
    }

    override protected[ziotest] def inAnyOrder[T](what: => T): T = {
      inContext(new UnorderedHandlers)(what)
    }

    private def inContext[T](context: Handlers)(what: => T): T = {
      currentExpectationContext.add(context)
      val prevContext = currentExpectationContext
      currentExpectationContext = context
      val r = what
      currentExpectationContext = prevContext
      r
    }
  }
} 