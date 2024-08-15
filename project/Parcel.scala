import play.sbt.PlayRunHook
import sbt._
import java.net.InetSocketAddress
import scala.sys.process.Process

object Parcel {
  def apply(base: File): PlayRunHook = {

    object ParcelProcess extends PlayRunHook {

      var watchProcess: Option[Process] = None

      override def beforeStarted(): Unit = {
        val npmInstall = "npm install --no-save"
        println(s"$npmInstall Started")
        Process(npmInstall, base / "builder").run().exitValue()
        println(s"$npmInstall Finished")
      }

      override def afterStarted(): Unit = {
        val parcelWatch = "npx parcel watch"
        println(s"$parcelWatch Starting...")
        watchProcess = Some(Process(parcelWatch, base / "builder").run)
      }

      override def afterStopped(): Unit = {
        watchProcess.map(p => p.destroy())
        watchProcess = None
      }
    }

    ParcelProcess
  }
}
