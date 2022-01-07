/**
 * BON in a Box - Script service
 */
package org.openapitools.server.apis

import com.google.gson.Gson
import io.ktor.application.*
import io.ktor.auth.*
import io.ktor.http.*
import io.ktor.response.*
import org.openapitools.server.Paths
import io.ktor.locations.*
import io.ktor.routing.*
import org.openapitools.server.infrastructure.ApiPrincipal
import org.openapitools.server.models.InlineResponse200
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


@KtorExperimentalLocationsAPI
fun Route.ScriptRunner(logger:Logger) {
    val gson = Gson()
    val empty = mutableMapOf<String, Any?>()

    get<Paths.getScriptInfo> {
        val exampleContentType = "text/plain"
        val exampleContentString = """http://server.com/scripts/somescript.md"""
        
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

    get<Paths.runScript> { parameters ->
        val exampleContentType = "application/json"
        val exampleContentString = """{
          "files" : {
            "presence" : "presence.tiff",
            "uncertainty" : "uncertainty.tiff"
          },
          "logs" : "Starting... Script completed!"
        }"""
        
        logger.info("scriptPath: ${parameters.scriptPath}")
        parameters.params?.forEach { param -> logger.info("param: $param") }
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

}
