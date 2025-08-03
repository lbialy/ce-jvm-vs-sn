//> using scala "3.7.1"
//> using toolkit default
//> using options "-deprecation" "-Wunused:all"

import os.{proc, pwd}

enum Runtime:
  case Jvm
  case Sn(mode: String, lto: String, gc: String)
  case GraalvmNativeImage

  override def toString: String =
    this match
      case Jvm                => "jvm"
      case Sn(mode, lto, gc)  => s"sn-$mode-$lto-$gc"
      case GraalvmNativeImage => "graalvm-ni"

  def bin: String =
    this match
      case Jvm                => "bench-jvm"
      case Sn(mode, lto, gc)  => s"bench-sn-${mode}-${lto}-${gc}"
      case GraalvmNativeImage => "bench-graalvm-ni"

  def packageArgs: Seq[String] =
    this match
      case Jvm => Seq("-f")
      case Sn(mode, lto, gc) =>
        Seq(
          "--platform",
          "scala-native",
          "--native-mode",
          mode,
          "--native-lto",
          lto,
          "--native-gc",
          gc,
          "-f" // overwrite
        )
      case GraalvmNativeImage =>
        Seq("--native-image", "-f", "--graalvm-args", "--no-fallback", "--graalvm-args", "--install-exit-handlers")

enum Result:
  case Measure(runtime: Runtime, queueCap: Int, ms: String)
  case Failed(runtime: Runtime, queueCap: Int, error: String)

def run(benchSrc: os.Path, runtime: Runtime, queueCaps: Seq[Int], cb: Int => Unit): Seq[Result] =
  val bin = runtime.bin
  val cmd = Seq(
    "scala",
    "package",
    benchSrc.toString,
    "-o",
    bin
  ) ++ runtime.packageArgs

  val packageOut = proc(cmd).call(cwd = pwd, check = false, stderr = os.Pipe)
  if packageOut.exitCode != 0 then
    cb(0)
    Seq(Result.Failed(runtime, 0, packageOut.out.text() + "\n" + packageOut.err.text()))
  else
    for queueCap <- queueCaps yield
      val execOut =
        proc(s"./$bin").call(cwd = pwd, check = false, stderr = os.Pipe, env = Map("CAP" -> queueCap.toString()))
      val stdOut = execOut.out.text()
      val stdErr = execOut.err.text()
      val res = stdOut.linesIterator
        .collectFirst { case s"concurrent producer→consumer (1000000 msgs, $cap queue cap): $ms ms" =>
          ms
        }
        .map(ms => Result.Measure(runtime, queueCap, ms))
        .getOrElse {
          Result.Measure(runtime, queueCap, s"could not find ms for queue cap $queueCap in:\n$stdOut\n$stdErr")
        }
      cb(queueCap)
      res

@main def matrix(): Unit =
  val queueCaps = Seq(1, 1024, 65534)
  val modes = Seq("debug", "release-fast", "release-size", "release-full")
  val ltos =
    val osName = System.getProperty("os.name").toLowerCase
    if osName.contains("mac") || osName.contains("darwin") then Seq("none", "full") // Filter out "thin" on macOS
    else Seq("none", "full", "thin")
  val gcs = Seq("immix", "commix", "boehm", "none")

  // change the benchmark here
  val benchSrc = pwd / "pipeline.scala"
  require(os.exists(benchSrc), s"$benchSrc not found – put the benchmark beside this script")

  println("complete matrix:")
  var matrixNo = 0
  for
    queueCap <- queueCaps
    mode <- modes
    lto <- ltos
    gc <- gcs
  do
    matrixNo += 1
    println(s"variant $matrixNo: queue cap: $queueCap, mode: $mode, lto: $lto, gc: $gc")
  for queueCap <- queueCaps do
    matrixNo += 1
    println(s"variant $matrixNo: queue cap: $queueCap, mode: jvm, lto: n/a, gc: parallel")
  for queueCap <- queueCaps do
    matrixNo += 1
    println(s"variant $matrixNo: queue cap: $queueCap, mode: graalvm-ni, lto: n/a, gc: parallel")

  val totalVariants = modes.length * ltos.length * gcs.length * queueCaps.length + (2 * queueCaps.length)
  var completedVariants = 0

  def updateProgress(runtime: Option[Runtime], queueCap: Option[Int]): Unit =
    val percentage = (completedVariants * 100) / totalVariants
    val progressBar = "█" * (percentage / 5) + "░" * (20 - percentage / 5)
    print("\r" + " " * 200)
    print(f"\r[$progressBar] $completedVariants/$totalVariants ($percentage%%) ${runtime
        .map(_.toString)
        .getOrElse("n/a")} ${queueCap.map(_.toString).getOrElse("n/a")}")
    if completedVariants == totalVariants then println()

  println()
  println("running matrix:")
  updateProgress(None, None)

  val snResults = for
    mode <- modes
    lto <- ltos
    gc <- gcs
    res <- run(
      benchSrc,
      Runtime.Sn(mode, lto, gc),
      queueCaps,
      queueCap => {
        completedVariants += 1
        updateProgress(Some(Runtime.Sn(mode, lto, gc)), Some(queueCap))
      }
    )
  yield res

  val jvmResults =
    run(
      benchSrc,
      Runtime.Jvm,
      queueCaps,
      queueCap => {
        completedVariants += 1
        updateProgress(Some(Runtime.Jvm), Some(queueCap))
      }
    )

  val graalvmNativeImageResults =
    run(
      benchSrc,
      Runtime.GraalvmNativeImage,
      queueCaps,
      queueCap => {
        completedVariants += 1
        updateProgress(Some(Runtime.GraalvmNativeImage), Some(queueCap))
      }
    )

  val results = snResults ++ jvmResults ++ graalvmNativeImageResults
  val measurements = results
    .collect[Result.Measure] { case Result.Measure(runtime, queueCap, ms) =>
      Result.Measure(runtime, queueCap, ms)
    }
    .sortBy(_.ms.toInt)

  // Print table
  val header = f"${"mode"}%-13s${"lto"}%-6s${"gc"}%-7s${"cap"}%7s${"ms"}%7s"
  println("-" * header.length)
  println(header)
  println("-" * header.length)

  for Result.Measure(runtime, queueCap, ms) <- measurements
  do
    runtime match
      case Runtime.Jvm                => println(f"${"jvm"}%-13s${"n/a"}%-6s${"n/a"}%-7s${queueCap}%7s${ms}%7s")
      case Runtime.Sn(mode, lto, gc)  => println(f"${mode}%-13s${lto}%-6s${gc}%-7s${queueCap}%7s${ms}%7s")
      case Runtime.GraalvmNativeImage => println(f"${"graalvm-ni"}%-13s${"n/a"}%-6s${"n/a"}%-7s${queueCap}%7s${ms}%7s")

  println()
  println("-" * header.length)
  println("failed:")
  println("-" * header.length)
  for Result.Failed(runtime, queueCap, error) <- results.collect[Result.Failed] {
      case Result.Failed(runtime, queueCap, error) => Result.Failed(runtime, queueCap, error)
    }
  do
    runtime match
      case Runtime.Jvm                => println(f"${"jvm"}%-13s${"n/a"}%-6s${"n/a"}%-7s${queueCap}%7s${error}")
      case Runtime.Sn(mode, lto, gc)  => println(f"${mode}%-13s${lto}%-6s${gc}%-7s${queueCap}%7s${error}")
      case Runtime.GraalvmNativeImage => println(f"${"graalvm-ni"}%-13s${"n/a"}%-6s${"n/a"}%-7s${queueCap}%7s${error}")
