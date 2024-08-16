import sbt.Keys._
import sbt._
import scala.sys.process.Process
import com.typesafe.sbt.packager.Keys.dist

/** Settings defined in this file are run as part of the dist stage.
  */
object ParcelBuild {
  val parcelBuild = TaskKey[Int]("parcel-build")

  val parcelBundleSetting: Seq[sbt.Def.Setting[_]] = Seq(
    parcelBuild := {
      Process("npm install --no-save", (Compile / baseDirectory).value / "builder") #&& Process(
        "npm run build",
        (Compile / baseDirectory).value / "builder"
      ) !
    },
    dist := { dist dependsOn parcelBuild }.value
  )
}
