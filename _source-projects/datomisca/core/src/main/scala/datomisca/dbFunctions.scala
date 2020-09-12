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


class AddDbFunction(
    val ident: Keyword,
    lang:      String,
    params:    Seq[String],
    code:      String,
    imports:   String    = "",
    requires:  String    = "",
    partition: Partition = Partition.USER
) extends TxData with KeywordIdentified {

  def toTxData: AnyRef =
    datomic.Util.map(
      Namespace.DB / "id",    DId(partition).toDatomicId,
      Namespace.DB / "ident", ident,
      Namespace.DB / "fn",    datomic.Peer.function(
        datomic.Util.map(
          AddDbFunction.lang,     lang,
          AddDbFunction.imports,  datomic.Util.read(imports),
          AddDbFunction.requires, datomic.Util.read(requires),
          AddDbFunction.params,   datomic.Util.list(params: _*),
          AddDbFunction.code,     code
        )
      )
    )

  override def toString = toTxData.toString
}

abstract class TypedAddDbFunction(fn: AddDbFunction) extends TxData with KeywordIdentified {
  override val ident = fn.ident

  def toTxData: AnyRef = fn.toTxData
}
// TypedAddDbFunction 0-22 in managed source

/*
 * Construct a vanila database function.
 */
object AddDbFunction {

  private val lang:     Keyword = clojure.lang.Keyword.intern(null, "lang")
  private val imports:  Keyword = clojure.lang.Keyword.intern(null, "imports")
  private val requires: Keyword = clojure.lang.Keyword.intern(null, "requires")
  private val params:   Keyword = clojure.lang.Keyword.intern(null, "params")
  private val code:     Keyword = clojure.lang.Keyword.intern(null, "code")

  def apply(kw: Keyword)
           (params: String*)
           (lang: String, partition: Partition = Partition.USER, imports: String = "", requires: String = "")
           (code: String) =
    new AddDbFunction(kw, lang, params, code, imports, requires, partition)
}
