package lavender

import java.nio.file.{Files, Path}

class LvSourcePath(cp: Path*) {
  def resolve(name: String): Option[Path] = {
    cp.map(_ resolve s"$name.lv").find(Files.exists(_))
  }
}
