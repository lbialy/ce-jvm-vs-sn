//> using scala "3.7.1"
//> using toolkit default
//> using options "-deprecation" "-Wunused:all"

import os.{proc, pwd}

enum Runtime:
  case Jvm
  case Sn(mode: String, lto: String, gc: String)
  case GraalvmNativeImage

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
  case Measure(runtime: Runtime, ms: String)
  case Failed(runtime: Runtime, error: String)

def run(benchSrc: os.Path, runtime: Runtime, cb: () => Unit): Result =
  val bin = runtime.bin
  val cmd = Seq(
    "scala-cli",
    "package",
    benchSrc.toString,
    "-o",
    bin
  ) ++ runtime.packageArgs

  val packageOut = proc(cmd).call(cwd = pwd, check = false, stderr = os.Pipe)
  if packageOut.exitCode != 0 then
    cb()
    Result.Failed(runtime, packageOut.out.text() + "\n" + packageOut.err.text())
  else
    val execOut = proc(s"./$bin").call(cwd = pwd, check = false, stderr = os.Pipe)
    val stdOut = execOut.out.text()
    val stdErr = execOut.err.text()
    val ms = stdOut.linesIterator
      .collectFirst { case s"concurrent producer→consumer (1000000 msgs, 1024 queue cap): $ms ms" =>
        ms
      }
      .getOrElse {
        throw Exception(s"could not find ms in:\n$stdOut\n$stdErr")
      }
    cb()
    Result.Measure(runtime, ms)

@main def matrix(): Unit =
  val modes = Seq("debug") // , "release-fast", "release-size", "release-full")
  val ltos = Seq("none") // , "full", "thin")
  val gcs = Seq("immix") // , "commix", "boehm", "none")

  // change the benchmark here
  val benchSrc = pwd / "pipeline.scala"
  require(os.exists(benchSrc), s"$benchSrc not found – put the benchmark beside this script")

  println("complete matrix:")
  for
    mode <- modes
    lto <- ltos
    gc <- gcs
  do println(s"mode: $mode, lto: $lto, gc: $gc")
  println("mode: jvm, lto: n/a, gc: parallel")
  println("mode: graalvm-ni, lto: n/a, gc: parallel")

  val totalVariants = modes.length * ltos.length * gcs.length + 2 // +1 for jvm, +1 for graalvm-ni
  var completedVariants = 0

  def updateProgress(): Unit =
    val percentage = (completedVariants * 100) / totalVariants
    val progressBar = "█" * (percentage / 5) + "░" * (20 - percentage / 5)
    print(f"\r[$progressBar] $completedVariants/$totalVariants ($percentage%%)")
    if completedVariants == totalVariants then println()

  println()
  println("running matrix:")
  updateProgress()

  val snResults = for
    mode <- modes
    lto <- ltos
    gc <- gcs
  yield run(
    benchSrc,
    Runtime.Sn(mode, lto, gc),
    () => {
      completedVariants += 1
      updateProgress()
    }
  )

  val jvmResult = run(
    benchSrc,
    Runtime.Jvm,
    () => {
      completedVariants += 1
      updateProgress()
    }
  )

  val graalvmNativeImageResult = run(
    benchSrc,
    Runtime.GraalvmNativeImage,
    () => {
      completedVariants += 1
      updateProgress()
    }
  )

  val results = (snResults ++ Seq(jvmResult) ++ Seq(graalvmNativeImageResult))
  val measurements = results
    .collect[Result.Measure] { case Result.Measure(runtime, ms) =>
      Result.Measure(runtime, ms)
    }
    .sortBy(_.ms)

  // Print table
  val header = f"${"mode"}%-13s${"lto"}%-6s${"gc"}%-7s${"ms"}%7s"
  println("-" * header.length)
  println(header)
  println("-" * header.length)

  for Result.Measure(runtime, ms) <- measurements
  do
    runtime match
      case Runtime.Jvm                => println(f"${"jvm"}%-13s${"n/a"}%-6s${"n/a"}%-7s${ms}%7s")
      case Runtime.Sn(mode, lto, gc)  => println(f"${mode}%-13s${lto}%-6s${gc}%-7s${ms}%7s")
      case Runtime.GraalvmNativeImage => println(f"${"graalvm-ni"}%-13s${"n/a"}%-6s${"n/a"}%-7s${ms}%7s")

  println()
  println("-" * header.length)
  println("failed:")
  println("-" * header.length)
  for Result.Failed(runtime, error) <- results.collect[Result.Failed] { case Result.Failed(runtime, error) =>
      Result.Failed(runtime, error)
    }
  do
    runtime match
      case Runtime.Jvm                => println(f"${"jvm"}%-13s${"n/a"}%-6s${"n/a"}%-7s${error}")
      case Runtime.Sn(mode, lto, gc)  => println(f"${mode}%-13s${lto}%-6s${gc}%-7s${error}")
      case Runtime.GraalvmNativeImage => println(f"${"graalvm-ni"}%-13s${"n/a"}%-6s${"n/a"}%-7s${error}")
