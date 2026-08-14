package io.genkidama.learn.java.helpdesk.http;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.genkidama.learn.java.helpdesk.domain.TicketService;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

class HelpDeskHttpServerTest {
    private final ObjectMapper json = new ObjectMapper();
    private final HttpClient client = HttpClient.newHttpClient();

    @Test
    void healthEndpointProvesTheApplicationCanServeHttp() throws Exception {
        try (var server = startedServer()) {
            HttpResponse<String> response = client.send(
                    HttpRequest.newBuilder(uri(server, "/health")).GET().build(),
                    HttpResponse.BodyHandlers.ofString());

            assertEquals(200, response.statusCode());
            assertEquals("ok", json.readTree(response.body()).get("status").asText());
        }
    }

    @Test
    void createsListsAndAdvancesATicketThroughJsonApi() throws Exception {
        try (var server = startedServer()) {
            HttpResponse<String> created = sendJson(server, "/api/tickets", """
                    {"title":"VPN caída","description":"Sin acceso remoto","priority":"HIGH"}
                    """);
            assertEquals(201, created.statusCode());
            JsonNode ticket = json.readTree(created.body());
            assertEquals("OPEN", ticket.get("status").asText());

            HttpResponse<String> listed = client.send(
                    HttpRequest.newBuilder(uri(server, "/api/tickets")).GET().build(),
                    HttpResponse.BodyHandlers.ofString());
            assertEquals(200, listed.statusCode());
            assertEquals(1, json.readTree(listed.body()).size());

            HttpResponse<String> advanced = sendJson(server, "/api/tickets/1/advance", "{}");
            assertEquals(200, advanced.statusCode());
            assertEquals("IN_PROGRESS", json.readTree(advanced.body()).get("status").asText());
        }
    }

    @Test
    void translatesDomainAndInputFailuresToUsefulHttpStatuses() throws Exception {
        try (var server = startedServer()) {
            HttpResponse<String> invalid = sendJson(server, "/api/tickets", """
                    {"title":"   ","priority":"NORMAL"}
                    """);
            assertEquals(400, invalid.statusCode());

            HttpResponse<String> missing = sendJson(server, "/api/tickets/999/advance", "{}");
            assertEquals(404, missing.statusCode());
        }
    }

    private HelpDeskHttpServer startedServer() throws Exception {
        var server = new HelpDeskHttpServer(new TicketService(), json, 0);
        server.start();
        return server;
    }

    private HttpResponse<String> sendJson(HelpDeskHttpServer server, String path, String body) throws Exception {
        return client.send(
                HttpRequest.newBuilder(uri(server, path))
                        .header("Content-Type", "application/json")
                        .POST(HttpRequest.BodyPublishers.ofString(body))
                        .build(),
                HttpResponse.BodyHandlers.ofString());
    }

    private URI uri(HelpDeskHttpServer server, String path) {
        return URI.create("http://127.0.0.1:" + server.port() + path);
    }
}
