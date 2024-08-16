import sbt.Keys._
import sbt._
import scala.sys.process.Process
import com.typesafe.sbt.packager.Keys.dist

/** Settings defined in this file are run as part of the dist stage.
  */
object ParcelBuild {
  val parcelBuild = TaskKey[Int]("parcel-build")

  private def runOperation(operation: String, result: Int): Int = {
    if (result != 0) {
      throw new Exception(s"$operation failed with result $result")
    }
    result
  }

  val parcelBundleSetting: Seq[sbt.Def.Setting[_]] = Seq(
    parcelBuild :=
      runOperation(
        "npx parcel build",
        Process("npx parcel build", (Compile / baseDirectory).value / "builder").run().exitValue()
      ),
    dist := { dist dependsOn parcelBuild }.value
  )
}
