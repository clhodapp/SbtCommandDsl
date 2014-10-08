
```
import sbt._
import Keys._

object Build extends sbt.Build {

  lazy val root = project.in(file(".")).settings(
    Keys.commands ++= Seq(testCommand1, testCommand2, testCommand3)
  )

  override lazy val projects = Seq(root)

  import CommandDsl._

  val testCommand1 = noArg("testCommand1")(
    for {
      version <- get(Keys.version)
      _ = println(s"version is now $version.")
    } yield ()
  )

  val testCommand2 = noArg("testCommand2")(
    for {
      svs <- get(scalaVersion)
      _ = println(s"scalaVersion is $svs.")
      ver <- get(version)
      _ = println(s"version is $ver.")
      _ = println(s"flipping...")
      _ <- set(scalaVersion := ver)
      _ = println(s"scalaVersion is $svs.")
      _ <- set(version := svs)
      _ = println(s"version is $ver.")
    } yield ()
  )

  val testCommand3 = singleArg("testCommand3") { arg =>
    for {
      version <- get(Keys.version)
      _ = println(s"version was $version. Setting to $arg")
      _ <- set(Keys.version := arg)
      version <- get(Keys.version)
      _ = println(s"version is now $version.")
    } yield ()
  }

}
```
