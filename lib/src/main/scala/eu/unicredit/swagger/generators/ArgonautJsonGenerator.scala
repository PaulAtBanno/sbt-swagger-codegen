/* Copyright 2015 UniCredit S.p.A.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.banno.swagger.generators

import eu.unicredit.swagger.SwaggerConversion
import eu.unicredit.swagger.generators.{ JsonGenerator, SyntaxString }
import treehugger.forest._
import definitions._
import io.swagger.parser.SwaggerParser
import treehuggerDSL._

import scala.collection.JavaConverters._

class ArgonautJsonGenerator extends JsonGenerator with SwaggerConversion {

  def generateJsonInit(packageName: String): String = {
    val initTree =
      BLOCK {
        Seq(IMPORT("argonaut._, Argonaut._"))
      } inPackage packageName

    treeToString(initTree)
  }

  def generateJsonImplicits(vds: List[ValDef]): String = {
    val tree = PACKAGEOBJECTDEF("json") := BLOCK(vds)

    treeToString(tree)
  }

  def generateJsonCodecs(fileName: String): List[ValDef] = {
    val swagger = new SwaggerParser().read(fileName)

    val models = swagger.getDefinitions.asScala

    (for {
      (name, model) <- models
    } yield {
      val properties = getProperties(model).toList
      VAL(s"${name}Codec", s"CodecJson[$name]") withFlags (Flags.IMPLICIT) :=
        REF(s"casecodec${properties.size}") APPLY (REF(s"{$name}.apply"), REF(s"{$name}.unapply")) APPLY properties.map(p => LIT(p._1))
    }).toList
  }

  def generateJson(destPackage: String, vds: List[ValDef]): Iterable[SyntaxString] = {
    val pre = generateJsonInit(destPackage)

    val tree = PACKAGEOBJECTDEF("json") := BLOCK(vds)

    val code = treeToString(tree)
    Seq(SyntaxString("json", pre, code))
  }

  def generate(fileName: String, destPackage: String): Iterable[SyntaxString] = {
    generateJson(destPackage, generateJsonCodecs(fileName))
  }
}
