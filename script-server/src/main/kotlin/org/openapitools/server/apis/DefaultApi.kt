/**
* BON in a Box - Script service
* No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
*
* The version of the OpenAPI document: 1.0.0
* Contact: jean-michel.lord@mcgill.ca
*
* NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
* https://openapi-generator.tech
* Do not edit the class manually.
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
import org.openapitools.server.models.Info
import org.openapitools.server.models.ScriptRunResult

@KtorExperimentalLocationsAPI
fun Route.DefaultApi() {
    val gson = Gson()
    val empty = mutableMapOf<String, Any?>()

    get<Paths.getPipelineInfo> {
        val exampleContentType = "application/json"
        val exampleContentString = """{
          "outputs" : {
            "key" : {
              "description" : "coordinate of occurances in",
              "label" : "occurences",
              "type" : "text/csv",
              "example" : "file.csv"
            }
          },
          "references" : [ {
            "text" : "Rick Bonney Expanding the Impact of Citizen Science BioScience Volume 71 Issue 5 May 2021 Pages 448–451",
            "doi" : "10.1093/biosci/biab041"
          }, {
            "text" : "Rick Bonney Expanding the Impact of Citizen Science BioScience Volume 71 Issue 5 May 2021 Pages 448–451",
            "doi" : "10.1093/biosci/biab041"
          } ],
          "inputs" : {
            "key" : {
              "description" : "scientific name of the species",
              "label" : "species names",
              "type" : "text",
              "example" : "Glyptemys insculpta"
            }
          },
          "description" : "This sample script shows how it works.",
          "script" : "HelloR.R",
          "external_link" : "https://github.com/GEO-BON/biab-2.0"
        }"""
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

    get<Paths.getPipelineOutputs> {
        val exampleContentType = "application/json"
        val exampleContentString = """{
          "id1" : "Folder/Script_ext/88204814d39b762f933d67d9a3d68832",
          "id2" : "Folder/Script2/1a015dccdfb7f639ec32c278506e7c5a"
        }"""
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

    get<Paths.getScriptInfo> {
        val exampleContentType = ""
        val exampleContentString = """"""
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

    get<Paths.pipelineListGet> {
        val exampleContentType = "application/json"
        val exampleContentString = """"[\"SHI.yml\",\"Folder>SDM.yml\"]""""
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

    post<Paths.runPipeline> {
        val exampleContentType = ""
        val exampleContentString = """"""
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

    post<Paths.runScript> {
        val exampleContentType = "application/json"
        val exampleContentString = """{
          "files" : {
            "presence" : "presence.tiff",
            "uncertainty" : "uncertainty.tiff"
          },
          "logs" : "Starting... Script completed!"
        }"""
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

    get<Paths.scriptListGet> {
        val exampleContentType = "application/json"
        val exampleContentString = """"[\"HelloWorld.yml\",\"RunSDM.yml\",\"SHI.yml\"]""""
        
        when (exampleContentType) {
            "application/json" -> call.respond(gson.fromJson(exampleContentString, empty::class.java))
            "application/xml" -> call.respondText(exampleContentString, ContentType.Text.Xml)
            else -> call.respondText(exampleContentString)
        }
    }

}
