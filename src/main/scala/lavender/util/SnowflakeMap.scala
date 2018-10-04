package lavender.util

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.HashMap
import scala.collection.{AbstractMap, MapLike, mutable}

class SnowflakeMap[K, +V] private (private val inner: Map[String, V])
  extends AbstractMap[SnowflakeType[K], V]
    with Map[SnowflakeType[K], V]
    with MapLike[SnowflakeType[K], V, SnowflakeMap[K, V]] {
  type Key = SnowflakeType[K]

  private def keyToSnowflake(k: String): Key = SnowflakeType[K](k)

  override def +[V1 >: V](kv: (SnowflakeType[K], V1)): SnowflakeMap[K, V1] = new SnowflakeMap(inner + kv)

  override def get(key: SnowflakeType[K]): Option[V] = inner.get(key)

  override def iterator: Iterator[(Key, V)] = inner.iterator.map {
    case (k, v) => (keyToSnowflake(k), v)
  }

  override def -(key: SnowflakeType[K]):  SnowflakeMap[K, V] = new SnowflakeMap(inner - key)

  override def empty: SnowflakeMap[K, V] = SnowflakeMap.empty
}

object SnowflakeMap {

  /**
    * Create an empty snowflake map.
    */
  def empty[K, V]: SnowflakeMap[K, V] = new SnowflakeMap(HashMap.empty)

  /**
    * Create a snowflake map from multiple values.
    */
  def apply[K, V](elems: (SnowflakeType[K], V)*): SnowflakeMap[K, V] =
    new SnowflakeMap(HashMap.apply(elems: _*))

  /**
    * Create a snowflake map from an iterable of snowflakes and values.
    */
  def apply[K, V](iterable: Iterable[(SnowflakeType[K], V)]): SnowflakeMap[K, V] = apply(iterable.toSeq: _*)

  /**
    * Create a snowflake map from an iterable of values while using a provided
    * function to get the key.
    */
  def withKey[K, V](iterable: Iterable[V])(f: V => SnowflakeType[K]): SnowflakeMap[K, V] =
    apply(iterable.map(v => f(v) -> v).toSeq: _*)

  implicit def canBuildFrom[S, A, B]: CanBuildFrom[SnowflakeMap[S, A], (SnowflakeType[S], B), SnowflakeMap[S, B]] =
    new CanBuildFrom[SnowflakeMap[S, A], (SnowflakeType[S], B), SnowflakeMap[S, B]] {
      override def apply(from: SnowflakeMap[S, A]): mutable.Builder[(SnowflakeType[S], B), SnowflakeMap[S, B]] = apply()
      override def apply(): mutable.Builder[(SnowflakeType[S], B), SnowflakeMap[S, B]] =
        new mutable.MapBuilder[SnowflakeType[S], B, SnowflakeMap[S, B]](empty)
    }
}
