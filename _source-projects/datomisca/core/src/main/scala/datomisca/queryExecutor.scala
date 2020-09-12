/*
 * Copyright 2012 Pellucid and Zenexity
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package datomisca

import scala.concurrent.blocking
import scala.util.control.NonFatal

import java.{util => ju}


class QueryProcessingException(rootCause: Throwable, chain: Seq[String])
  extends DatomiscaException(s"""query failed with root cause: ${rootCause.getMessage}; in processing chain: ${chain.mkString("[", "; ", "]")}""", rootCause)

class QueryException(cause: Throwable)
  extends DatomiscaException(s"query failed with cause: ${cause.getMessage}", cause)


private[datomisca] trait QueryExecutor extends TypedQueryExecutor


/* DATOMIC QUERY */
private[datomisca] object QueryExecutor {

  private def execQuery(q: AbstractQuery, in: Seq[AnyRef]): ju.Collection[ju.List[AnyRef]] =
    try {
      blocking {
        datomic.Peer.q(q.query, in: _*)
      }
    } catch {
      case ex: Throwable if ex.getMessage startsWith "processing" =>
        val builder = Seq.newBuilder[String]
        var e = ex
        while (e.getCause != null) {
          builder += e.getMessage
          e = e.getCause
        }
        throw new QueryProcessingException(e, builder.result)
      case NonFatal(ex) =>
        throw new QueryException(ex)
    }

  /*
  private[datomisca] def directQuery(q: AbstractQuery, in: Seq[AnyRef]) =
    new Iterable[IndexedSeq[DatomicData]] {
      private val jColl: ju.Collection[ju.List[AnyRef]] = execQuery(q, in)
      override def iterator = new Iterator[IndexedSeq[DatomicData]] {
        private val jIter: ju.Iterator[ju.List[AnyRef]] = jColl.iterator
        override def hasNext = jIter.hasNext
        override def next() = new IndexedSeq[DatomicData] {
          private val jList: ju.List[AnyRef] = jIter.next()
          override def length = jList.size
          override def apply(idx: Int): DatomicData =
            DatomicData.toDatomicData(jList.get(idx))
          override def iterator = new Iterator[DatomicData] {
            private val jIter: ju.Iterator[AnyRef] = jList.iterator
            override def hasNext = jIter.hasNext
            override def next() = DatomicData.toDatomicData(jIter.next)
          }
        }
      }
    }
  */

  private[datomisca] def execute[OutArgs](q: AbstractQuery, in: Seq[AnyRef])(implicit outConv: QueryResultToTuple[OutArgs]): Iterable[OutArgs] = {
    new Iterable[OutArgs] {
      private val jColl: ju.Collection[ju.List[AnyRef]] = execQuery(q, in)
      override def isEmpty = jColl.isEmpty
      override def size = jColl.size
      override def iterator = new Iterator[OutArgs] {
        private val jIter: ju.Iterator[ju.List[AnyRef]] = jColl.iterator
        override def hasNext = jIter.hasNext
        override def next() = outConv.toTuple(jIter.next())
      }
    }
  }
}

private[datomisca] trait QueryResultToTuple[T] {
  def toTuple(l: ju.List[AnyRef]): T
}

private[datomisca] object QueryResultToTuple extends QueryResultToTupleInstances
