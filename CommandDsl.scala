
import sbt._

object StateM {
  case class State[S, A](run: S => (S, A)) {
    def map[B](f: A => B): State[S, B] = State { s =>
      val (newS, a) = run(s)
      (newS, f(a))
    }
    def flatMap[B](f: A => State[S, B]): State[S, B] = State { (s: S) =>
      val (newS, a) = run(s)
      f(a).run(newS)
    }
  }
  def get[S]: State[S, S] = State(s => (s, s))
  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def unit[S, A](a: A): State[S, A] = State(s => (s, a))
}

object CommandDsl {

  type St[A] = StateM.State[sbt.State, A]

  private[this] def extracted[T](f: sbt.Extracted => T): St[T] = StateM.get.map { sbtState =>
    f(Project.extract(sbtState))
  }

  def append(settings: Seq[Def.Setting[_]]): St[Unit] = for {
    sbtState <- StateM.get[sbt.State]
    extracted <- StateM.unit(Project.extract(sbtState))
    _ <- StateM.put(extracted.append(settings, sbtState))
  } yield ()

  def set(settings:Def.Setting[_]*): St[Unit] =
    append(settings)

  def currentLoader: St[ClassLoader] = extracted(_.currentLoader)

  def currentProject: St[ResolvedProject] = extracted(_.currentProject)

  def currentRef: St[ProjectRef] = extracted(_.currentRef)

  def currentUnit: St[LoadedBuildUnit] = extracted(_.currentUnit)

  def get[T](key: SettingKey[T]): St[T] = extracted(_.get(key))

  def get[T](key: TaskKey[T]): St[Task[T]] = extracted(_.get(key))

  def getOpt[T](key: SettingKey[T]): St[Option[T]] = extracted(_.getOpt(key))

  def rootProject[T]: St[URI => String] = extracted(_.rootProject)

  def runAggregated[T](key: TaskKey[T]): St[Unit] = for {
    sbtState <- StateM.get[sbt.State]
    extracted <- StateM.unit(Project.extract(sbtState))
    newState <- StateM.unit(extracted.runAggregated(key, sbtState))
    _ <- StateM.put(newState)
  } yield ()


  def runTask[T](key: TaskKey[T]): St[T] = for {
    sbtState <- StateM.get[sbt.State]
    extracted <- StateM.unit(Project.extract(sbtState))
    r <- StateM.unit(extracted.runTask(key, sbtState))
    (newState, result) = r
    _ <- StateM.put(newState)
  } yield result

  def session: St[SessionSettings] = extracted(_.session)

  def structure: St[BuildStructure] = extracted(_.structure)

  def noArg(commandName: String)(f: StateM.State[sbt.State, Unit]): Command =
    Command.command(commandName)(sbtState => f.run(sbtState)._1)

  def singleArg(commandName: String)(f: String => StateM.State[sbt.State, Unit]): Command =
    Command.single(commandName)((sbtState, arg) => f(arg).run(sbtState)._1)

  def multiArg(commandName: String, argsHint: String)(f: Seq[String] => StateM.State[sbt.State, Unit]): Command =
    Command.args(commandName, argsHint)((sbtState, args) => f(args).run(sbtState)._1)

}

